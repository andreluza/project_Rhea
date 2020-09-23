### codigo para arrumar os dados para o winbugs
rm(list=ls())
#####################
## dados de deteccao
#####################

require (here) # para transitar entre pastas
require(vegan) # para padronizar os dados
library(R2WinBUGS) # para rodar o modelo 

## gbif

load(here("deteccoes", "Gbif", "input_GBIF.RData"))

## inat
load(here("deteccoes", "outras_bases_INAT", "input_INAT.RData"))

## vertnet
load(here("deteccoes", "vertnet", "input_VERTNET.RData"))

## ebird
load(here("deteccoes", "ebird", "INPUT_ebird.RData"))


# formato tabela
y.ebird <- unname (as.matrix (df_mun_rhea))


summary (

    unlist(
  
  
        lapply (seq (1,nrow(y.ebird)), function (i) 
  
      
            length(y.ebird [i,] [which (is.na (y.ebird [i,]) != T)])

    
            )
  
        )

    )

y.ebird [is.na(y.ebird)] <- 0

## padronizacoes e zaz
dist.ebird <- as.matrix(df_mun_distance)
dist.ebird [is.na(dist.ebird)] <-0
dist.ebird <- unname(dist.ebird)
dist.duration <- as.matrix(df_mun_duration)
dist.duration [is.na(dist.duration)] <-0
dist.duration <- unname(dist.duration)
dist.observers <- as.matrix(df_mun_observers)
dist.observers [is.na(dist.observers)] <-0  
dist.observers <- unname(dist.observers)

## wikiaves
load(here("deteccoes", "wikiaves", "input_wikiaves.RData"))


#####################
## dados espaciais
#####################

# carregar shapefile RS
require(rgdal)
require(raster)
require(rgeos)

load("shapeRS_lambert.RData")
## area do mun em km

area_mun <- gArea (shape_RS,byid=T)/1000.00
## CONFERIR AQUI file:///C:/Users/topoa/Downloads/Ranking_RS_2009-2010_Alfabetico.pdf
area_mun [which(shape_RS$NM_MUNICIP == "AMARAL FERRADOR")]

#####################
## dados de covariaveis
#####################

load(here("covariaveis", "dados_covariaveis.RData"))

## padronizacoes e zaz
bovinos <-   (ifelse (is.na(numero_bovinos_corte$n_cabecas),0,numero_bovinos_corte$n_cabecas))
ovelha <- (numero_ovelhas$Ovinos)
#soja <-  (sqrt(numero_estabelecimentos_lavoura$Soja_em_grao),"standardize")[,1]

plot(area_mun,numero_ovelhas$Ovinos)
plot(area_mun,numero_bovinos_corte$n_cabecas)
plot(area_mun,numero_estabelecimentos_lavoura$Soja_em_grao)

campo <- usos_tabela$`Campo seco.area` + 
                      usos_tabela$`Campo umido.area` +
                      usos_tabela$`Campo de feixe de restinga.area`+
                      usos_tabela$`Campo em regeneracao.area`

floresta <- usos_tabela$`Mata com ate 30% de antropico.area`+
                       usos_tabela$`Mata nativa.area`+
                       usos_tabela$`Mata com ate 50% de antropico.area`+
                       usos_tabela$Silvicultura.area

agricultura_uso <- usos_tabela$`Agricultura de sequeiro.area`
plot(area_mun,agricultura_uso)

### padronizacoes
## area de habitat campestre e florestal por municipio
## multiplicar a cobertura de habitat pela area do municipio
area_campo <- area_mun*campo
habitat_campo <- area_mun*campo
habitat_floresta <- area_mun*floresta
habitat_lavoura <- area_mun*agricultura_uso

plot(area_mun,habitat_lavoura)
plot(area_mun,habitat_floresta)
plot(area_mun,habitat_campo)


## densidade de gado no habitat da ema
## multiplicar o numero de cabeacas de gado pela cobertura de campo

densidade_gado <- bovinos*campo
densidade_ovelha <- ovelha*campo
plot(area_mun,densidade_gado)
plot(area_mun,densidade_ovelha)

## raiz quadrada, e entao padronizar pela media e sd
habitat_campo <- decostand (sqrt(habitat_campo),"standardize")
habitat_floresta <- decostand (sqrt(habitat_floresta),"standardize")
habitat_lavoura <- decostand (sqrt(habitat_lavoura),"standardize")
densidade_gado <- decostand (sqrt(densidade_gado),"standardize")
densidade_ovelha <- decostand (sqrt(densidade_ovelha),"standardize")

plot(area_mun,
     c(usos_tabela$`Campo seco.area` + 
         usos_tabela$`Campo umido.area` +
         usos_tabela$`Campo de feixe de restinga.area`+
         usos_tabela$`Campo em regeneracao.area`)*area_mun)

plot(area_mun,habitat_floresta)
plot(area_mun,densidade_gado)

plot(area_mun,
     c(usos_tabela$`Mata com ate 30% de antropico.area`+
         usos_tabela$`Mata nativa.area`+
         usos_tabela$`Mata com ate 50% de antropico.area`+
         usos_tabela$Silvicultura.area)*area_mun)


###
shape_RS@data$campo <- habitat_campo

cores_sem <- data.frame (cores= log(area_campo),
                         NM_MUNICIP=shape_RS$NM_MUNICIP)

require(ggplot2)
f.mun_sem<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_sem<- cbind (f.mun_sem, 
                   Nespecies= cores_sem [match (f.mun_sem$id, cores_sem$NM_MUNICIP),]$cores)

## inserir vals
c_sem <-   ggplot() + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Area of grassland habitat, per municipality") +
  scale_fill_gradient2 (low='white', high='darkred', na.value = "white",
                        limits=c(-5,max(cores_sem$cores)), 
                        breaks=seq(-5,max(cores_sem$cores,na.rm=T),by=2),
                        name="Area (km2, log scale)") ## para continuo

c_sem + theme_classic()

#####################
## dados espaciais - vizinhanca
#####################

load(here("covariaveis", "CARparams_10km.RData"))

### modelo

sink("static_model_data_integration_no_cattle.txt")
cat("
    model {
    

    ###################################
    ###### SPATIAL MODEL  #############
    ###################################

    # CAR prior - spatial random effect

    for(j in 1:sumNeigh){ # rm isso para o modelo sem espaco
       weights[j] <- 1
    }

    # rm isso para o modelo sem espaco
    spacesigma ~ dunif(0,5)
    spacetau <- 1/(spacesigma*spacesigma)
    SPACE [1:nCell] ~ car.normal(adj[],weights[],num[],spacetau)
    
    ###################################
    ###### ECOLOGICAL MODEL ###########
    ###################################

    # PRIORS
    BETA0 ~ dunif(-10,10)
    #BETA.BOVINO ~ dunif(−10, 10)
    #BETA.OVELHA ~ dunif(−10, 10)
    #BETA.LAVOURA ~ dunif(−10, 10)
    BETA.CAMPO ~ dunif(−10, 10)
    #BETA.FLORESTA ~ dunif(−10, 10)

    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + #BETA.BOVINO * bovino[i] + 
                      #BETA.OVELHA * ovelha[i]+ 
                      #BETA.LAVOURA * lavoura[i]+ 
                      BETA.CAMPO * campo[i] +
                      #BETA.FLORESTA * floresta[i] +
                      SPACE [cell.id[i]] # # rm isso para o modelo sem espaco

     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    for (i in 1:nsite) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])


    }

    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ## EBIRD
  
    for (i in 1:nsite) {
      for (j in 1:nrepEB) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

    ### INAT
    
    for (i in 1:nsite) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    for (i in 1:nsite) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    
    for (i in 1:nsite) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
    ##############################
    #### GOODNESS-OF-FIT #########
    ##############################
    
    ## not implemented yet
        
    ##############################
    #### DERIVED PARAMETERS ######
    ##############################
    
    #compute the mean detection probability of each dataset: 
    muP1 <- mean(P1[])
    muP2 <- mean(P2[,])
    muP3 <- mean(P3[])
    muP4 <- mean(P4[])
    muP5 <- mean(P5[])

    ## Number of occupied municipalities (finite sample size)
    fs.z <- sum(z[])/nsite
    

    }## end of the model
    ",fill = TRUE)
sink()

## initial values
inits = function() {list(z = rep (1, nrow(y.ebird)),
                         ALPHA.GBIF= rnorm (1,mean=10),
                         ALPHA.DIST = rnorm (1,mean=2),
                         ALPHA.DURATION = rnorm (1,mean=20),
                         ALPHA.OBSERVER = rnorm (1,mean=2),
                         ALPHA.INAT = rnorm (1,mean=10),
                         ALPHA.VERTNET = rnorm (1,mean=10),
                         ALPHA.PICT = rnorm (1,mean=10),
                         ALPHA.SONG = rnorm (1,mean=10))}

## parametros a serem monitorados
params.espaco <- c("BETA0", #"BETA.BOVINO", "BETA.OVELHA",
                   #"BETA.LAVOURA",
                   "BETA.CAMPO",
                   #"BETA.FLORESTA",
            "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
            "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            "muP1","muP2","muP3","muP4","muP5",
            "spacesigma","SPACE",
            "z","psi","fs.z"
            )

## MCMC settings
######################
## short form
#na <- 10; nb <- 15; ni <- 20; nc <- 2; nt <- 1

# long form
ni <- 60000; na <- 30000; nb <- 50000; nc <- 3; nt <- 10

## 


#### bound data (with space)

str(win.data.space <- list(nsite= dim(dados_det_ema_gbif)[1],
                     y.gbif = dados_det_ema_gbif[,"det"],
                     nSP.gbif = dados_det_ema_gbif [,"riqueza_aves"], 
                     y.inat= dados_det_ema_inat [,"det"],
                     nSP.inat = dados_det_ema_inat [,"riqueza_aves"], 
                     y.vertnet= dados_det_ema_vertnet [,"det"],
                     nSP.vertnet = dados_det_ema_vertnet [,"riqueza_aves"],
                     y.wikiaves = dados_wikiaves [, "RHAMERICANA"],
                     nPIC.wikiaves = dados_wikiaves [, "NPIC"],
                     nSONG.wikiaves = dados_wikiaves [, "NSONG"],
                     adj = adj, num = num, sumNeigh = sumNeigh,
                     cell.id=cell.id,
                     nCell = nrow(hex.centroids),
                     yEB = y.ebird [,1:11],
                     dist = dist.ebird [,1:11],
                     duration = dist.duration [,1:11],
                     observer = dist.observers [,1:11],
                     nrepEB = ncol(df_mun_observers [,1:11]),
                     #bovino = densidade_gado[,1],
                     #ovelha = densidade_ovelha[,1],
                     #lavoura= habitat_lavoura[,1],
                     campo=habitat_campo[,1]
                     #floresta=habitat_floresta[,1]
))

out_espaco_10km_sem_gado <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
            model.file = "static_model_data_integration_no_cattle.txt", 
            inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            codaPkg=F, DIC=TRUE, debug=TRUE,
            bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

save(out_espaco_10km_sem_gado,file="Rhea_out_espaco_10km.RData")

### 25 km cell size

#####################
## dados espaciais
#####################

load(here("covariaveis", "CARparams_25km.RData"))

#### bound data (with space)

str(win.data.space <- list(nsite= dim(dados_det_ema_gbif)[1],
                           y.gbif = dados_det_ema_gbif[,"det"],
                           nSP.gbif = dados_det_ema_gbif [,"riqueza_aves"], 
                           y.inat= dados_det_ema_inat [,"det"],
                           nSP.inat = dados_det_ema_inat [,"riqueza_aves"], 
                           y.vertnet= dados_det_ema_vertnet [,"det"],
                           nSP.vertnet = dados_det_ema_vertnet [,"riqueza_aves"],
                           y.wikiaves = dados_wikiaves [, "RHAMERICANA"],
                           nPIC.wikiaves = dados_wikiaves [, "NPIC"],
                           nSONG.wikiaves = dados_wikiaves [, "NSONG"],
                           adj = adj, num = num, sumNeigh = sumNeigh,
                           cell.id=cell.id,
                           nCell = nrow(hex.centroids),
                           yEB = y.ebird [,1:11],
                           dist = dist.ebird [,1:11],
                           duration = dist.duration [,1:11],
                           observer = dist.observers [,1:11],
                           nrepEB = ncol(df_mun_observers [,1:11]),
                           #bovino = densidade_gado[,1],
                           #ovelha = densidade_ovelha[,1],
                           #lavoura= habitat_lavoura[,1],
                           campo=habitat_campo[,1]
                           #floresta=habitat_floresta[,1]
))

out_espaco_25km_sem_gado <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
                                 model.file = "static_model_data_integration_no_cattle.txt", 
                                 inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                 codaPkg=F, DIC=TRUE, debug=TRUE,
                                 bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

save(out_espaco_25km_sem_gado,file="Rhea_out_espaco_25km.RData")


### 50 km cell size

#####################
## dados espaciais
#####################

load(here("covariaveis", "CARparams_50km.RData"))

#### bound data (with space)

str(win.data.space <- list(nsite= dim(dados_det_ema_gbif)[1],
                           y.gbif = dados_det_ema_gbif[,"det"],
                           nSP.gbif = dados_det_ema_gbif [,"riqueza_aves"], 
                           y.inat= dados_det_ema_inat [,"det"],
                           nSP.inat = dados_det_ema_inat [,"riqueza_aves"], 
                           y.vertnet= dados_det_ema_vertnet [,"det"],
                           nSP.vertnet = dados_det_ema_vertnet [,"riqueza_aves"],
                           y.wikiaves = dados_wikiaves [, "RHAMERICANA"],
                           nPIC.wikiaves = dados_wikiaves [, "NPIC"],
                           nSONG.wikiaves = dados_wikiaves [, "NSONG"],
                           adj = adj, num = num, sumNeigh = sumNeigh,
                           cell.id=cell.id,
                           nCell = nrow(hex.centroids),
                           yEB = y.ebird [,1:11],
                           dist = dist.ebird [,1:11],
                           duration = dist.duration [,1:11],
                           observer = dist.observers [,1:11],
                           nrepEB = ncol(df_mun_observers [,1:11]),
                           #bovino = densidade_gado[,1],
                           #ovelha = densidade_ovelha[,1],
                           #lavoura= habitat_lavoura[,1],
                           campo=habitat_campo[,1]
                           #floresta=habitat_floresta[,1]
))

out_espaco_50km_sem_gado <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
                        model.file = "static_model_data_integration_no_cattle.txt", 
                        inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                        codaPkg=F, DIC=TRUE, debug=TRUE,
                        bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

save(out_espaco_50km_sem_gado,file="Rhea_out_espaco_50km.RData")

#### without space
#### bound data (with space)

str(win.data <- list(nsite= dim(dados_det_ema_gbif)[1],
                     y.gbif = dados_det_ema_gbif[,"det"],
                     nSP.gbif = dados_det_ema_gbif [,"riqueza_aves"], 
                     y.inat= dados_det_ema_inat [,"det"],
                     nSP.inat = dados_det_ema_inat [,"riqueza_aves"], 
                     y.vertnet= dados_det_ema_vertnet [,"det"],
                     nSP.vertnet = dados_det_ema_vertnet [,"riqueza_aves"],
                     y.wikiaves = dados_wikiaves [, "RHAMERICANA"],
                     nPIC.wikiaves = dados_wikiaves [, "NPIC"],
                     nSONG.wikiaves = dados_wikiaves [, "NSONG"],
                     yEB = y.ebird [,1:11],
                     dist = dist.ebird [,1:11],
                     duration = dist.duration [,1:11],
                     observer = dist.observers [,1:11],
                     nrepEB = ncol(df_mun_observers [,1:11]),
                     #bovino = densidade_gado[,1],
                     #ovelha = densidade_ovelha[,1],
                     #lavoura= habitat_lavoura[,1],
                     campo=habitat_campo[,1]
                     #floresta=habitat_floresta[,1]
))

params <- c("BETA0", #"BETA.BOVINO", "BETA.OVELHA",
            #"BETA.LAVOURA",
            "BETA.CAMPO",#"BETA.FLORESTA",
            "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
            "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            "muP1","muP2","muP3","muP4","muP5",
            "z","psi","fs.z"
)

out_sem_espaco_sem_gado <- bugs(data = win.data, parameters.to.save = params, 
                                     model.file = "static_model_data_integration_sem_espaco.txt", 
                                     inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     codaPkg=F, DIC=TRUE, debug=TRUE,
                                     bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

save(out_sem_espaco_sem_gado,file="out_sem_espaco.RData")


