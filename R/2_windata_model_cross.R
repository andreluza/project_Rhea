# -------------------------------------------------
# load packages
source ("R/packages.R")

#---------------------
# load Detection and observation data
#---------------------
load(here("data","organized_data", "input_GBIF.RData")) ## gbif
load(here("data","organized_data", "input_INAT.RData")) ## inat
load(here("data","organized_data", "input_VERTNET.RData")) ## vertnet
load(here("data","organized_data", "INPUT_ebird.RData")) ## ebird
load(here("data","organized_data", "input_wikiaves.RData")) # wikiaves

#-------------------------
# load spatial data
#--------------------------
load(here ("data","geo","shapeRS_lambert.RData")) # shapefile of Rio Grande do Sul
area_mun <- gArea (shape_RS,byid=T)/1000.00## municipality_area, km
## check areas here file:///C:/Users/topoa/Downloads/Ranking_RS_2009-2010_Alfabetico.pdf
area_mun [which(shape_RS$NM_MUNICIP == "AMARAL FERRADOR")] # match

#-------------------------------
# site covariates (municipality)
#-------------------------------
load(here("data","organized_data", "dados_covariaveis.RData"))

# aggregate grassland data
campo <- usos_tabela$`Campo seco.area` + 
                      usos_tabela$`Campo umido.area` +
                      usos_tabela$`Campo de feixe de restinga.area`+
                      usos_tabela$`Campo em regeneracao.area`
# agriculture
agricultura_uso <- usos_tabela$`Agricultura de sequeiro.area`

# Number of properties per municipality
cercas <- numero_estabelecimentos_lavoura$Total # proxy for fence

# correlation between covariates is low
cor(data.frame(cercas, campo, agricultura_uso))
# check municipality order
# numero_estabelecimentos_lavoura$X == rownames(usos_tabela)

### Standardize covariates
## grassland area/agriculture/N properties per municipality
## grassland cover * municipality area
area_campo <- area_mun*campo
habitat_campo <- area_mun*campo
area_lavoura <- area_mun*agricultura_uso
habitat_lavoura <- area_mun*agricultura_uso
propriedades_relativo_area <- cercas/area_mun

## raiz quadrada, e entao padronizar pela media e sd
habitat_campo <- decostand (sqrt(habitat_campo),"standardize")
habitat_lavoura <- decostand (sqrt(habitat_lavoura),"standardize")
cercas_std <- decostand ((propriedades_relativo_area),"standardize")

# creaste a folder to host output
dir.create('output')

# bind grassland data into the shape
shape_RS@data$campo <- area_campo
# plot
cores_sem <- data.frame (cores= sqrt(area_campo),
                         NM_MUNICIP=shape_RS$NM_MUNICIP)
# data for mapping
f.mun_sem<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_sem<- cbind (f.mun_sem, 
                   Nespecies= cores_sem [match (f.mun_sem$id, cores_sem$NM_MUNICIP),]$cores)

# grassland
## insert vals
c_campo <-   ggplot() + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Grassland area, per municipality") +
  scale_fill_gradient2 (low='white', high='#206A5D', na.value = "white",
                        limits=c(0,max(cores_sem$cores)), 
                        breaks=seq(0,max(cores_sem$cores,na.rm=T),by=500),
                        name=expression(sqrt("Area (km2)"))) ## para continuo

(c_campo <- c_campo + theme_classic() + 
    theme (axis.text = element_text(size=6),
           axis.title = element_text(size=8),
           legend.text = element_text(size=8),
           legend.title = element_text(size=9))+
    xlab("Longitude") + 
    ylab("Latitude")) 

## agriculture
# bind grassland data into the shape
shape_RS@data$agricultura <- area_lavoura
# plot
cores_sem <- data.frame (cores= sqrt(area_lavoura),
                         NM_MUNICIP=shape_RS$NM_MUNICIP)

# data for map
f.mun_sem<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_sem<- cbind (f.mun_sem, 
                   Nespecies= cores_sem [match (f.mun_sem$id, cores_sem$NM_MUNICIP),]$cores)

## insert vals
c_agri <-   ggplot() + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                       color=Nespecies, fill=Nespecies), 
                                   colour = NA, size=1) + 
  labs (title= "Crop field area, per municipality") +
  scale_fill_gradient2 (low='white', high='#E48900', na.value = "white",
                        limits=c(0,max(cores_sem$cores)), 
                        breaks=seq(0,max(cores_sem$cores,na.rm=T),by=500),
                        name=expression(sqrt("Area (km2)"))) ## para continuo

(c_agri<-c_agri + theme_classic() + 
    theme (axis.text = element_text(size=6),
           axis.title = element_text(size=8),
           legend.text = element_text(size=8),
           legend.title = element_text(size=9))+
  xlab("Longitude") + 
  ylab("Latitude"))

# cercas
# bind numerb of rural properties data into the shape
shape_RS@data$cercas <- propriedades_relativo_area

# plot
cores_sem <- data.frame (cores= (propriedades_relativo_area),
                         NM_MUNICIP=shape_RS$NM_MUNICIP)

# data
f.mun_sem<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_sem<- cbind (f.mun_sem, 
                   Nespecies= cores_sem [match (f.mun_sem$id, cores_sem$NM_MUNICIP),]$cores)

## insert vals
c_cercas <-   ggplot() + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                        color=Nespecies, fill=Nespecies), 
                                    colour = NA, size=1) + 
  labs (title= "Number of rural properties, per municipality") +
  scale_fill_gradient2 (low='white', high='blue', na.value = "white",
                        limits=c(0,max(cores_sem$cores)), 
                        breaks=seq(0,max(cores_sem$cores,na.rm=T),
                                   by=0.01),
                        name=expression(("# properties/area"))) ## para continuo

(c_cercas<-c_cercas + theme_classic() + 
    theme (axis.text = element_text(size=6),
           axis.title = element_text(size=8),
           legend.text = element_text(size=8),
           legend.title = element_text(size=9))+
    xlab("Longitude") + 
    ylab("Latitude"))


# bind maps and save
pdf (here ("output", "FigS1-3.pdf"),heigh=11,width=5)
grid.arrange(c_campo,
             c_agri,
             c_cercas)

dev.off()


#################################################################################
# ----------------------------------------------------------------------------- #

# MODELS

load(here("data","organized_data", "CARparams.RData")) # abrir dados espaciais

# MCMC settings

## short form
# na <- 10; nb <- 15; ni <- 20; nc <- 2; nt <- 1

# long form
ni <- 60000; nb <- 40000; na <- 30000;  nt <- 20; nc <- 3

# define a subset of ebird data -- too much missing lists

n_so_ebird <- round (mean(unlist(
  lapply (seq(1,nrow(y.ebird)), function (i) 
    sum(is.na(y.ebird[i,])!=T)))
  )
  )

pdf (here ("output", "FigS1-2.pdf"),height=5,width=5)
# histograma
hist(unlist(
  lapply (seq(1,nrow(y.ebird)), function (i) 
    sum(is.na(y.ebird[i,])!=T))),
  xlab="Number of checklists",main="")
abline(v=n_so_ebird,lwd=2)
dev.off()

# ----------------------------------------
# formating data to the models
# Diving the data set in test(validation) / training data
# Cross-validation involves taking out part of the municipalities from all data sets
# Here we will take 20% of the municipalities ~100 of 497 municipalities
library(dplyr)
#sampled <- sample_n(ID_MUN, size=100, replace=F) #aprox 20% of the muni, sample without replacement
#for each data set, take out the samples from munis that are in 'sampled' object:
load(here ("output","sampled.RData"))
#save(sampled, file = here ("output","sampled.RData"))
# GBIF
datGBOut <- dados_det_ema_gbif[dados_det_ema_gbif$mun %in% sampled$NM_MUNICIP, ]
datGBIn <- dados_det_ema_gbif[-which(dados_det_ema_gbif$mun %in% sampled$NM_MUNICIP), ]

# INAT
datINTOut <- dados_det_ema_inat[dados_det_ema_inat$mun %in% sampled$NM_MUNICIP, ]
datINTIn <- dados_det_ema_inat[-which(dados_det_ema_inat$mun %in% sampled$NM_MUNICIP), ]

# VERTNET
datVEROut <- dados_det_ema_vertnet[dados_det_ema_vertnet$mun %in% sampled$NM_MUNICIP, ]
datVERIn <- dados_det_ema_vertnet[-which(dados_det_ema_vertnet$mun %in% sampled$NM_MUNICIP), ]

# WIKIAVES
datWAOut <- dados_wikiaves[dados_wikiaves$MUNI %in% sampled$NM_MUNICIP, ]
datWAIn <- dados_wikiaves[-which(dados_wikiaves$MUNI %in% sampled$NM_MUNICIP), ]

# EBIRD
row.names(y.ebird) <- ID_MUN$NM_MUNICIP
datEBOut <- y.ebird[rownames(y.ebird) %in% sampled$NM_MUNICIP, ]
datEBIn <- y.ebird[-which(rownames(y.ebird) %in% sampled$NM_MUNICIP),]

# Ebird effort covariates:
# distance
row.names(dist.ebird) <- ID_MUN$NM_MUNICIP
dist.ebirdOUT <- dist.ebird[rownames(dist.ebird) %in% sampled$NM_MUNICIP, ]
dist.ebird <- dist.ebird[-which(rownames(dist.ebird) %in% sampled$NM_MUNICIP),]
# duration
row.names(dist.duration) <- ID_MUN$NM_MUNICIP
dist.durationOUT <- dist.duration[rownames(dist.duration) %in% sampled$NM_MUNICIP, ]
dist.duration <- dist.duration[-which(rownames(dist.duration) %in% sampled$NM_MUNICIP),]
# observers
row.names(dist.observers) <- ID_MUN$NM_MUNICIP
dist.observersOUT <- dist.observers[rownames(dist.observers) %in% sampled$NM_MUNICIP, ]
dist.observers <- dist.observers[-which(rownames(dist.observers) %in% sampled$NM_MUNICIP),]

## save data for validation
save (datGBOut,datGBIn,datINTOut,datINTIn, datVEROut, datVERIn,datWAOut,datWAIn,datEBOut,datEBIn,
	file = here ("output", "validation_data.RData"))


## initial values (they work for all models)
inits = function() {list(z = rep (1, nrow(y.ebird)),
                         ALPHA.GBIF= rnorm (1,mean=10),
                         ALPHA.DIST = rnorm (1,mean=2),
                         ALPHA.DURATION = rnorm (1,mean=20),
                         ALPHA.OBSERVER = rnorm (1,mean=2),
                         ALPHA.INAT = rnorm (1,mean=10),
                         ALPHA.VERTNET = rnorm (1,mean=10),
                         ALPHA.PICT = rnorm (1,mean=10),
                         ALPHA.SONG = rnorm (1,mean=10))}


# ------------------------------------- #
# null model without space and without grasslands
# create a folder to host bugs code
dir.create("bugs_code")
      
# write model
sink(here ("bugs_code","static_model_DI_null.txt"))
cat("

    model {
    
    ###################################
    ###### ECOLOGICAL MODEL ###########
    ###################################

    # PRIORS
    psi ~ dunif(0,1)
    
    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi) # True occupancy z at site i
  
    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

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


# bound data
str(win.data <- list(nsite = dim(dados_det_ema_gbif)[1],
                     y.gbif = datGBIn[,"det"],
                     nsiteIN1 = dim(datGBIn)[1],
                     nsiteOUT1 = dim(datGBOut)[1],
                     nSP.gbif = datGBIn[,"riqueza_aves"],
                     nSP.gbif6 = datGBOut[,"riqueza_aves"],
                     y.inat = datINTIn[,"det"],
                     nsiteIN2 = dim(datINTIn)[1],
                     nsiteOUT2 = dim(datINTOut)[1],
                     nSP.inat = datINTIn[,"riqueza_aves"], 
                     nSP.inat7 = datINTOut[,"riqueza_aves"],
                     y.vertnet = datVERIn [,"det"],
                     nsiteIN3 = dim(datVERIn)[1],
                     nsiteOUT3 = dim(datVEROut)[1],
                     nSP.vertnet = datVERIn[,"riqueza_aves"],
                     nSP.vertnet8 = datVEROut[,"riqueza_aves"],
                     nsiteIN4 = dim(datWAIn)[1],
                     nsiteOUT4 = dim(datWAOut)[1],
                     y.wikiaves = datWAIn[, "RHAMERICANA"],
                     nPIC.wikiaves = datWAIn[, "NPIC"],
                     nSONG.wikiaves = datWAIn[, "NSONG"],
                     nPIC.wikiaves9 = datWAOut[, "NPIC"],
                     nSONG.wikiaves9 = datWAOut[, "NSONG"],
                     yEB = datEBIn [,1:n_so_ebird],
                     nsiteOUT5 = dim(datEBOut)[1],
                     nsiteIN5 = dim(datEBIn)[1],
                     dist = dist.ebird [,1:n_so_ebird],
                     duration = dist.duration [,1:n_so_ebird],
                     observer = dist.observers [,1:n_so_ebird],
                     dist10 = dist.ebirdOUT [,1:n_so_ebird],
                     duration10 = dist.durationOUT [,1:n_so_ebird],
                     observer10 = dist.observersOUT [,1:n_so_ebird],
                     nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
                     nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird])
))

params <- c("ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
            "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            "muP1","muP2","muP3","muP4","muP5",
            "z","psi","fs.z",
            "y.gbif6", "y.inat7", 
            "y.vertnet8", "y.wikiaves9", "yEB10"
)

# run model
out_null_model1 <- bugs(data = win.data, 
                 parameters.to.save = params, 
                 model.file = here ("bugs_code","static_model_DI_null.txt"), 
                 inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                 codaPkg=F, DIC=TRUE, debug=F,
                 bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")


# save it
save (out_null_model1,file=here("output", "out_null_model1.RData"))


# ----------------------------------------------------------------------------------- #
# model 2 - spatial model (only space)


sink(here ("bugs_code","static_model_DI_autocorrelation.txt"))
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
    BETA.CAMPO ~ dunif(-10, 10)
    BETA.AGRI ~ dunif (-10,10)
    
    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + SPACE [cell.id[i]] # 

     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

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


## parametros a serem monitorados
params.espaco <- c("BETA0",
                  "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
                   "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
                   "muP1","muP2","muP3","muP4","muP5",
                   "spacesigma","SPACE",
                   "z","psi","fs.z",
                  "y.gbif6", "y.inat7", 
                  "y.vertnet8", "y.wikiaves9", "yEB10"
)

# bound data (with space) and report results (list across spatial data (different cell sizes))
out_model2  <- lapply (seq (1,3), function (i) { # 
  
  
  (win.data.space <- list(nsite = dim(dados_det_ema_gbif)[1],
                          y.gbif = datGBIn[,"det"],
                          nsiteIN1 = dim(datGBIn)[1],
                          nsiteOUT1 = dim(datGBOut)[1],
                          nSP.gbif = datGBIn[,"riqueza_aves"],
                          nSP.gbif6 = datGBOut[,"riqueza_aves"],
                          y.inat = datINTIn[,"det"],
                          nsiteIN2 = dim(datINTIn)[1],
                          nsiteOUT2 = dim(datINTOut)[1],
                          nSP.inat = datINTIn[,"riqueza_aves"], 
                          nSP.inat7 = datINTOut[,"riqueza_aves"],
                          y.vertnet = datVERIn [,"det"],
                          nsiteIN3 = dim(datVERIn)[1],
                          nsiteOUT3 = dim(datVEROut)[1],
                          nSP.vertnet = datVERIn[,"riqueza_aves"],
                          nSP.vertnet8 = datVEROut[,"riqueza_aves"],
                          nsiteIN4 = dim(datWAIn)[1],
                          nsiteOUT4 = dim(datWAOut)[1],
                          y.wikiaves = datWAIn[, "RHAMERICANA"],
                          nPIC.wikiaves = datWAIn[, "NPIC"],
                          nSONG.wikiaves = datWAIn[, "NSONG"],
                          nPIC.wikiaves9 = datWAOut[, "NPIC"],
                          nSONG.wikiaves9 = datWAOut[, "NSONG"],
                          adj = adj[[i]], 
                          num = num[[i]], 
                          sumNeigh = sumNeigh[[i]],
                          cell.id=cell.id[[i]],
                          nCell = nrow(hex.centroids[[i]]),
                          yEB = datEBIn [,1:n_so_ebird],
                          nsiteOUT5 = dim(datEBOut)[1],
                          nsiteIN5 = dim(datEBIn)[1],
                          dist = dist.ebird [,1:n_so_ebird],
                          duration = dist.duration [,1:n_so_ebird],
                          observer = dist.observers [,1:n_so_ebird],
                          dist10 = dist.ebirdOUT [,1:n_so_ebird],
                          duration10 = dist.durationOUT [,1:n_so_ebird],
                          observer10 = dist.observersOUT [,1:n_so_ebird],
                          nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
                          nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird])
  ))
  
  
  tryCatch ( # try overcome errors
    out_model2 <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
                                model.file = here ("bugs_code","static_model_DI_autocorrelation.txt"), 
                                inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                codaPkg=F, DIC=TRUE, debug=T,
                                bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS"),
    error = function (e)
      return (e))
  ; # return
  out_model2
}
)


# save it
save (out_model2,file=here("output", 
                           "out_model2.RData"))


# ----------------------------------------------------------------------------------- #
# model 3 (only grassland cover)

# write model
sink(here ("bugs_code","static_model_DI_grassland.txt"))
cat("

    model {
    
    ###################################
    ###### ECOLOGICAL MODEL ###########
    ###################################

    # PRIORS
    BETA0 ~ dunif(-10,10)
    BETA.CAMPO ~ dunif(-10, 10)

    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + BETA.CAMPO * campo [i]

     # Keeping Winbugs on the track
     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

  
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

# bound data
str(win.data <- list(nsite = dim(dados_det_ema_gbif)[1],
                     y.gbif = datGBIn[,"det"],
                     nsiteIN1 = dim(datGBIn)[1],
                     nsiteOUT1 = dim(datGBOut)[1],
                     nSP.gbif = datGBIn[,"riqueza_aves"],
                     nSP.gbif6 = datGBOut[,"riqueza_aves"],
                     y.inat = datINTIn[,"det"],
                     nsiteIN2 = dim(datINTIn)[1],
                     nsiteOUT2 = dim(datINTOut)[1],
                     nSP.inat = datINTIn[,"riqueza_aves"], 
                     nSP.inat7 = datINTOut[,"riqueza_aves"],
                     y.vertnet = datVERIn [,"det"],
                     nsiteIN3 = dim(datVERIn)[1],
                     nsiteOUT3 = dim(datVEROut)[1],
                     nSP.vertnet = datVERIn[,"riqueza_aves"],
                     nSP.vertnet8 = datVEROut[,"riqueza_aves"],
                     nsiteIN4 = dim(datWAIn)[1],
                     nsiteOUT4 = dim(datWAOut)[1],
                     y.wikiaves = datWAIn[, "RHAMERICANA"],
                     nPIC.wikiaves = datWAIn[, "NPIC"],
                     nSONG.wikiaves = datWAIn[, "NSONG"],
                     nPIC.wikiaves9 = datWAOut[, "NPIC"],
                     nSONG.wikiaves9 = datWAOut[, "NSONG"],
                     yEB = datEBIn [,1:n_so_ebird],
                     nsiteOUT5 = dim(datEBOut)[1],
                     nsiteIN5 = dim(datEBIn)[1],
                     dist = dist.ebird [,1:n_so_ebird],
                     duration = dist.duration [,1:n_so_ebird],
                     observer = dist.observers [,1:n_so_ebird],
                     dist10 = dist.ebirdOUT [,1:n_so_ebird],
                     duration10 = dist.durationOUT [,1:n_so_ebird],
                     observer10 = dist.observersOUT [,1:n_so_ebird],
                     nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
                     nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird]),
                     campo=habitat_campo[,1]
))

params <- c("BETA0", 
            "BETA.CAMPO",
            "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
            "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            "muP1","muP2","muP3","muP4","muP5",
            "z","psi","fs.z",
            "y.gbif6", "y.inat7", 
            "y.vertnet8", "y.wikiaves9", "yEB10"
)

out_grass_model3 <- bugs(data = win.data, parameters.to.save = params, 
                                model.file = here ("bugs_code","static_model_DI_grassland.txt"), 
                                inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                codaPkg=F, DIC=T, debug=F,
                                bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

# save it
save (out_grass_model3,file=here("output", "out_grass_model3.RData"))


# ----------------------------------------------------------------------------------- #
# model 4 - grassland and agriculture, no autocorrelation

# write model
sink(here ("bugs_code","static_model_DI_grassland_agriculture.txt"))
cat("

    model {
    
    ###################################
    ###### ECOLOGICAL MODEL ###########
    ###################################

    # PRIORS
    BETA0 ~ dunif(-10,10)
    BETA.CAMPO ~ dunif(-10, 10)
    BETA.AGRI ~ dunif(-10, 10)
    BETA.FENCE ~ dunif(-10, 10)

    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + BETA.CAMPO * campo [i] + BETA.AGRI * agri [i] + BETA.FENCE * cercas[i]

     # Keeping Winbugs on the track
     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

  
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


# bound data
str(win.data <- list(nsite = dim(dados_det_ema_gbif)[1],
                     y.gbif = datGBIn[,"det"],
                     nsiteIN1 = dim(datGBIn)[1],
                     nsiteOUT1 = dim(datGBOut)[1],
                     nSP.gbif = datGBIn[,"riqueza_aves"],
                     nSP.gbif6 = datGBOut[,"riqueza_aves"],
                     y.inat = datINTIn[,"det"],
                     nsiteIN2 = dim(datINTIn)[1],
                     nsiteOUT2 = dim(datINTOut)[1],
                     nSP.inat = datINTIn[,"riqueza_aves"], 
                     nSP.inat7 = datINTOut[,"riqueza_aves"],
                     y.vertnet = datVERIn [,"det"],
                     nsiteIN3 = dim(datVERIn)[1],
                     nsiteOUT3 = dim(datVEROut)[1],
                     nSP.vertnet = datVERIn[,"riqueza_aves"],
                     nSP.vertnet8 = datVEROut[,"riqueza_aves"],
                     nsiteIN4 = dim(datWAIn)[1],
                     nsiteOUT4 = dim(datWAOut)[1],
                     y.wikiaves = datWAIn[, "RHAMERICANA"],
                     nPIC.wikiaves = datWAIn[, "NPIC"],
                     nSONG.wikiaves = datWAIn[, "NSONG"],
                     nPIC.wikiaves9 = datWAOut[, "NPIC"],
                     nSONG.wikiaves9 = datWAOut[, "NSONG"],
                     yEB = datEBIn [,1:n_so_ebird],
                     nsiteOUT5 = dim(datEBOut)[1],
                     nsiteIN5 = dim(datEBIn)[1],
                     dist = dist.ebird [,1:n_so_ebird],
                     duration = dist.duration [,1:n_so_ebird],
                     observer = dist.observers [,1:n_so_ebird],
                     dist10 = dist.ebirdOUT [,1:n_so_ebird],
                     duration10 = dist.durationOUT [,1:n_so_ebird],
                     observer10 = dist.observersOUT [,1:n_so_ebird],
                     nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
                     nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird]),
                     campo=habitat_campo[,1],
                     agri = habitat_lavoura[,1],
                     cercas = cercas_std[,1]
))

params <- c("BETA0", 
            "BETA.CAMPO","BETA.AGRI","BETA.FENCE",
            "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
            "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            "muP1","muP2","muP3","muP4","muP5",
            "z","psi","fs.z",
            "y.gbif6", "y.inat7", 
            "y.vertnet8", "y.wikiaves9", "yEB10"
)

out_grass_agri_model4 <- bugs(data = win.data, parameters.to.save = params, 
                                model.file = here ("bugs_code","static_model_DI_grassland_agriculture.txt"), 
                                inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                codaPkg=F, DIC=TRUE, debug=F,
                                bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS")

# save it
save (out_grass_agri_model4,file=here("output", "out_grass_agri_model4.RData"))


# ----------------------------------------------------------------------------------- #
# model 5 - complete model, with grassland and autocorrelation

sink(here ("bugs_code","static_model_DI_grassland_autocorrelation.txt"))
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
    BETA.CAMPO ~ dunif(-10, 10)
    
    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + BETA.CAMPO * campo[i] +
                      SPACE [cell.id[i]] # # rm isso para o modelo sem espaco

     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

  
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


## parametros a serem monitorados
params.espaco <- c("BETA0",
                   "BETA.CAMPO",
 	                "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
        	         "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
            	   "muP1","muP2","muP3","muP4","muP5",
            	   "spacesigma","SPACE",
            	   "z","psi","fs.z",
            	   "y.gbif6", "y.inat7", 
            	   "y.vertnet8", "y.wikiaves9", "yEB10"
            	   )


# bound data (with space) and report results (list across spatial data (different cell sizes))
out_model5  <- lapply (seq (1,3), function (i) {#
		
   
	(win.data.space <- list(nsite = dim(dados_det_ema_gbif)[1],
	                        y.gbif = datGBIn[,"det"],
	                        nsiteIN1 = dim(datGBIn)[1],
	                        nsiteOUT1 = dim(datGBOut)[1],
	                        nSP.gbif = datGBIn[,"riqueza_aves"],
	                        nSP.gbif6 = datGBOut[,"riqueza_aves"],
	                        y.inat = datINTIn[,"det"],
	                        nsiteIN2 = dim(datINTIn)[1],
	                        nsiteOUT2 = dim(datINTOut)[1],
	                        nSP.inat = datINTIn[,"riqueza_aves"], 
	                        nSP.inat7 = datINTOut[,"riqueza_aves"],
	                        y.vertnet = datVERIn [,"det"],
	                        nsiteIN3 = dim(datVERIn)[1],
	                        nsiteOUT3 = dim(datVEROut)[1],
	                        nSP.vertnet = datVERIn[,"riqueza_aves"],
	                        nSP.vertnet8 = datVEROut[,"riqueza_aves"],
	                        nsiteIN4 = dim(datWAIn)[1],
	                        nsiteOUT4 = dim(datWAOut)[1],
	                        y.wikiaves = datWAIn[, "RHAMERICANA"],
	                        nPIC.wikiaves = datWAIn[, "NPIC"],
	                        nSONG.wikiaves = datWAIn[, "NSONG"],
	                        nPIC.wikiaves9 = datWAOut[, "NPIC"],
	                        nSONG.wikiaves9 = datWAOut[, "NSONG"],
	                        adj = adj[[i]], 
	                        num = num[[i]], 
	                        sumNeigh = sumNeigh[[i]],
	                        cell.id=cell.id[[i]],
	                        nCell = nrow(hex.centroids[[i]]),
	                        yEB = datEBIn [,1:n_so_ebird],
	                        nsiteOUT5 = dim(datEBOut)[1],
	                        nsiteIN5 = dim(datEBIn)[1],
	                        dist = dist.ebird [,1:n_so_ebird],
	                        duration = dist.duration [,1:n_so_ebird],
	                        observer = dist.observers [,1:n_so_ebird],
	                        dist10 = dist.ebirdOUT [,1:n_so_ebird],
	                        duration10 = dist.durationOUT [,1:n_so_ebird],
	                        observer10 = dist.observersOUT [,1:n_so_ebird],
	                        nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
	                        nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird]),
	                        campo=habitat_campo[,1]
                     ))
  
	tryCatch ( # try overcome errors
	  out_model5 <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
            model.file = here ("bugs_code","static_model_DI_grassland_autocorrelation.txt"), 
            inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            codaPkg=F, DIC=TRUE, debug=F,
            bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS"),
    error = function (e)
      return (e))
	; # return
  out_model5
	}
)

# save it
save (out_model5,file=here("output", "out_model5.RData"))

# ----------------------------------------------------------------------------------- #
# model 6 - complete model, with grassland, agriculture and autocorrelation

sink(here ("bugs_code","static_model_DI_complete.txt"))
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
    BETA.CAMPO ~ dunif(-10,10)
    BETA.AGRI ~ dunif (-10,10)
    BETA.FENCE  ~ dunif (-10,10) 
    
    # LIKELIHOOD
    for (i in 1:nsite) {

      z[i] ~ dbern(psi[i]) # True occupancy z at site i
  
     mu[i] <- BETA0 + BETA.CAMPO * campo[i] +
                      BETA.AGRI * agri [i] +
                      BETA.FENCE * cercas [i] +
                      SPACE [cell.id[i]] 

     mu.lim[i] <- min(10, max(-10, mu[i]))  
     logit(psi[i]) <- mu.lim[i]

    }    

    ###################################
    ###### OBSERVATION MODELS #########
    ###################################

    ### GBIF 
    
    #treino - IN
    for (i in 1:nsiteIN1) {

        e1[i] <- ALPHA.GBIF * nSP.gbif[i]
        P1[i] <- 1-pow((1-0.5), e1[i])
        zP1[i] <- P1[i] * z [i]
        y.gbif [i] ~ dbern(zP1[i])

    }
    
    #teste - OUT
    for (i in 1:nsiteOUT1) {

        e6[i] <- ALPHA.GBIF * nSP.gbif6[i]
        P6[i] <- 1-pow((1-0.5), e6[i])
        zP6[i] <- P6[i] * z [i]
        y.gbif6 [i] ~ dbern(zP6[i])

    }


    # PRIOR FOR RICHNESS EFFECT
    ALPHA.GBIF ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    
    ### INAT
    
    #treino - IN
    
    for (i in 1:nsiteIN2) {
    
        e3[i] <- ALPHA.INAT * nSP.inat[i]
        P3[i] <- 1-pow((1-0.5), e3[i])
        zP3[i] <- P3[i] * z [i]
        y.inat [i] ~ dbern (zP3[i])
    
    }
    
    #teste - OUT
    
    for (i in 1:nsiteOUT2) {
    
        e7[i] <- ALPHA.INAT * nSP.inat7[i]
        P7[i] <- 1-pow((1-0.5), e7[i])
        zP7[i] <- P7[i] * z [i]
        y.inat7 [i] ~ dbern (zP7[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.INAT ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### VERTNET
    
    #treino IN
    
    for (i in 1:nsiteIN3) {
    
        e4[i] <- ALPHA.INAT * nSP.vertnet[i]
        P4[i] <- 1-pow((1-0.5), e4[i])
        zP4[i] <- P4[i] * z [i]
        y.vertnet [i] ~ dbern(zP4[i])
    
    }
  
    #teste OUT
    
    for (i in 1:nsiteOUT3) {
    
        e8[i] <- ALPHA.INAT * nSP.vertnet8[i]
        P8[i] <- 1-pow((1-0.5), e8[i])
        zP8[i] <- P8[i] * z [i]
        y.vertnet8 [i] ~ dbern(zP8[i])
    
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.VERTNET ~ dnorm(0,0.0001)I(0,1000)# truncated in positive values
    
    ### WIKIAVES
    #treino IN:
    
    for (i in 1:nsiteIN4) {
    
        e5[i] <- ALPHA.PICT * nPIC.wikiaves[i]+ALPHA.SONG*nSONG.wikiaves[i]
        P5[i] <- 1-pow((1-0.5), e5[i])
        zP5[i] <- P5[i] * z [i]
        y.wikiaves[i] ~ dbern(zP5[i])#+(1-zP5[i])*q[i])
        
    }
    
    #teste OUT:
    
    for (i in 1:nsiteOUT4) {
    
        e9[i] <- ALPHA.PICT * nPIC.wikiaves9[i]+ALPHA.SONG*nSONG.wikiaves9[i]
        P9[i] <- 1-pow((1-0.5), e9[i])
        zP9[i] <- P9[i] * z [i]
        y.wikiaves9[i] ~ dbern(zP9[i])#+(1-zP9[i])*q[i])
        
    }
    
    # PRIOR FOR RICHNESS EFFECT
    ALPHA.PICT ~ dnorm(0,0.0001)I(0,30000)# truncated in positive values
    ALPHA.SONG ~ dnorm(0,0.0001)I(0,10000)# truncated in positive values
  
  
  
  ## EBIRD
  
    for (i in 1:nsiteIN5) {
      for (j in 1:nrepEBin) {

        e2[i,j] <- ALPHA.DIST * dist[i,j] +
                   ALPHA.DURATION * duration[i,j] +
                   ALPHA.OBSERVER * observer[i,j]
 
        P2[i,j] <- 1-pow((1-0.5), e2[i,j])
        zP2[i,j] <- P2[i,j] * z [i]
        yEB [i,j] ~ dbern(zP2[i,j])

      }
    }
    
    for (i in 1:nsiteOUT5) {
      for (j in 1:nrepEBout) {

        e10[i,j] <- ALPHA.DIST * dist10[i,j] +
                   ALPHA.DURATION * duration10[i,j] +
                   ALPHA.OBSERVER * observer10[i,j]
 
        P10[i,j] <- 1-pow((1-0.5), e10[i,j])
        zP10[i,j] <- P10[i,j] * z [i]
        yEB10 [i,j] ~ dbern(zP10[i,j])

      }
    }
    
    ALPHA.DIST ~ dnorm(0,0.0001)I(0,1000)
    ALPHA.DURATION ~ dnorm(0,0.0001)I(0,2000)
    ALPHA.OBSERVER  ~ dnorm(0,0.0001)I(0,1000)

    
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


## parametros a serem monitorados
params.espaco <- c("BETA0",
                   "BETA.CAMPO","BETA.AGRI","BETA.FENCE",
                   "ALPHA.GBIF","ALPHA.DIST", "ALPHA.DURATION","ALPHA.OBSERVER",
                   "ALPHA.INAT", "ALPHA.VERTNET","ALPHA.PICT", "ALPHA.SONG", 
                   "muP1","muP2","muP3","muP4","muP5",
                   "spacesigma","SPACE",
                   "z","psi","fs.z",
                   "y.gbif6", "y.inat7", 
                   "y.vertnet8", "y.wikiaves9", "yEB10"
)

# bound data (with space) and report results (list across spatial data (different cell sizes))
out_complete_model6  <- lapply (seq (1,3), function (i) {# length(adj)
  
  
  (win.data.space <- list(nsite = dim(dados_det_ema_gbif)[1],
                          y.gbif = datGBIn[,"det"],
                          nsiteIN1 = dim(datGBIn)[1],
                          nsiteOUT1 = dim(datGBOut)[1],
                          nSP.gbif = datGBIn[,"riqueza_aves"],
                          nSP.gbif6 = datGBOut[,"riqueza_aves"],
                          y.inat = datINTIn[,"det"],
                          nsiteIN2 = dim(datINTIn)[1],
                          nsiteOUT2 = dim(datINTOut)[1],
                          nSP.inat = datINTIn[,"riqueza_aves"], 
                          nSP.inat7 = datINTOut[,"riqueza_aves"],
                          y.vertnet = datVERIn [,"det"],
                          nsiteIN3 = dim(datVERIn)[1],
                          nsiteOUT3 = dim(datVEROut)[1],
                          nSP.vertnet = datVERIn[,"riqueza_aves"],
                          nSP.vertnet8 = datVEROut[,"riqueza_aves"],
                          nsiteIN4 = dim(datWAIn)[1],
                          nsiteOUT4 = dim(datWAOut)[1],
                          y.wikiaves = datWAIn[, "RHAMERICANA"],
                          nPIC.wikiaves = datWAIn[, "NPIC"],
                          nSONG.wikiaves = datWAIn[, "NSONG"],
                          nPIC.wikiaves9 = datWAOut[, "NPIC"],
                          nSONG.wikiaves9 = datWAOut[, "NSONG"],
                          adj = adj[[i]], 
                          num = num[[i]], 
                          sumNeigh = sumNeigh[[i]],
                          cell.id=cell.id[[i]],
                          nCell = nrow(hex.centroids[[i]]),
                          yEB = datEBIn [,1:n_so_ebird],
                          nsiteOUT5 = dim(datEBOut)[1],
                          nsiteIN5 = dim(datEBIn)[1],
                          dist = dist.ebird [,1:n_so_ebird],
                          duration = dist.duration [,1:n_so_ebird],
                          observer = dist.observers [,1:n_so_ebird],
                          dist10 = dist.ebirdOUT [,1:n_so_ebird],
                          duration10 = dist.durationOUT [,1:n_so_ebird],
                          observer10 = dist.observersOUT [,1:n_so_ebird],
                          nrepEBin = ncol(dist.observers [,1:n_so_ebird]),
                          nrepEBout = ncol(dist.observersOUT [,1:n_so_ebird]),
                          campo=habitat_campo[,1],
                          agri = habitat_lavoura[,1],
                          cercas = cercas_std[,1]
  ))
  
  tryCatch ( # try overcome errors
    out_complete_model6 <- bugs(data = win.data.space, parameters.to.save = params.espaco, 
                                model.file = here ("bugs_code","static_model_DI_complete.txt"), 
                                inits = inits, n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                codaPkg=F, DIC=TRUE, debug=F,
                                bugs.directory="C:/Program Files/WinBUGS14/", program= "WinBUGS"),
    error = function (e)
      return (e))
  ; # return
  out_complete_model6
}
)

# save it
save (out_complete_model6,file=here("output", "out_complete_model6.RData"))




rm(list=ls())

