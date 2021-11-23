## mapping

## pacote para readOGR
require(rgdal)
require(raster)
require(here)
shape_RS <- readOGR(dsn=here("data","shape_munRS"), 
                    layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## obter os lagos para pintar diferente depois
lagos <- shape_RS [c(96,250),]
## remover os lagos
shape_RS <- shape_RS [-c(96,250),]

## shape south america
southAme<- readOGR(dsn=here ("data","South_America"),encoding="latin1", layer="South_America")
BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(BR_AR_URU)<-crs(shape_RS)

## carregar os resultados dos modelos
require(jagsUI)
require(R2WinBUGS)
list.res <- list.files (here("output"), pattern = "out_*")
#load all res at once
load (here ("output", list.res[1]))
load (here ("output", list.res[2]))
load (here ("output", list.res[3]))
load (here ("output", list.res[4]))
load (here ("output", list.res[5]))
load (here ("output", list.res[6]))

# list of all res
res_occ_models<- list (null=out_null_model1,
                       spatial10=out_model2[[1]],
                       spatial25=out_model2[[2]],
                       grass=out_grass_model3,
                       grass_agri=out_grass_agri_model4,
                       grass_aut10=out_model5[[1]],
                       grass_aut25=out_model5[[2]],
                       complete10=out_complete_model6[[1]],
                       complete25=out_complete_model6[[2]])
# df for model ranking
# loade deviance
load(here("output","deviance.RData"))

# estimates of number occupied municipalities (true occupancy)
z_per_model<-cbind (
  do.call (rbind, lapply (res_occ_models, function (i) 
    i$summary[grep("fs.z", rownames(i$summary)),])),
  
  do.call (rbind, lapply (res_occ_models, function (i) 
    sd(i$mean$z)))
)

# bind z into DIC table
deviance_models<-cbind (deviance_sel,
                   z_per_model )

# rank
deviance_models <- deviance_models[order (deviance_models[,'deviance_sel'],decreasing=F),]
best_ranked_mod <- which (deviance_models[,'deviance_sel'] == min (deviance_models[,'deviance_sel']))
deviance_models_subset <- deviance_models[best_ranked_mod,] # set of best ranked models

# correlation between estimates from diff models
require(corrplot)
par(mfrow=c(1,1),mar=c(5,5,5,5))
corrplot(cor(do.call(cbind,lapply (res_occ_models, function (i) i$mean$z))),
         main="Correlation between estimates of z",
         tl.cex = 0.7,
         mar = c(1, 1, 1, 1))


########################################
## interpretação dos coeficientes
########################################

coeficientes <- lapply (seq (1,length(res_occ_models)), function (i) {
  tab_coef <- res_occ_models[[i]]$summary [grep("BETA", rownames(res_occ_models[[i]]$summary)),c("mean", "sd","2.5%", "97.5%")]
  tab_coef <- cbind (tab_coef,mod=names(res_occ_models[i]))
  
})
# trabalhar somente com modelos com coeficientes das covariaveis de sitio campo e agricultura
# derreter esta lista para ter os coefs por modelo
coeficientes <- data.frame(do.call(rbind, coeficientes[4:9]))
colnames(coeficientes)[3:4]<-c("lower","upper")# ajustar noems das colunas 
coeficientes$mean<- as.numeric(coeficientes$mean)
coeficientes$sd<- as.numeric(coeficientes$sd)
coeficientes$lower<- as.numeric(coeficientes$lower)
coeficientes$upper<- as.numeric(coeficientes$upper)
# ajustar fator hab
coeficientes$covariate <- rownames(coeficientes) 
coeficientes$covariate [grep ("CAMPO",coeficientes$covariate)] <- "Campo"
coeficientes$covariate [grep ("AGRI",coeficientes$covariate)] <- "Agricultura"
coeficientes$covariate [grep ("BETA0",coeficientes$covariate)] <- "Intercepto"

## plot
require(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

pd=position_dodge(0.5)

p1 <- ggplot (coeficientes [which(coeficientes$covariate != "Intercepto"  & 
                                    coeficientes$mod %in% best_ranked_mod ),],  
              aes (y=as.numeric(mean), x=mod, fill=covariate,
                   group = NULL,
                   colour=covariate))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.5,position=pd,size=1) +
  geom_line(position=pd) +
  geom_point(position=pd, aes(colour=covariate),size=2.5)  

p1 <-p1 + scale_color_manual(values=c("#FB9300", "#206A5D"))

r <- p1 + scale_y_continuous(limits=c(-0.5,5.5),breaks=-2:5.5) + 
  geom_hline(yintercept=0, color="gray10", size=1,alpha=0.4)+
  #coord_cartesian(ylim=c(-1.1,1.1)) +
  theme(plot.margin=unit(c(0,0.5,0,0),"lines"))
r

s <-r +  theme_classic()  + #coord_flip(ylim=c(-1,5))+
  scale_x_discrete()+
  theme(axis.text.x = element_text(angle=45,
                                   hjust = 1,
                                   size=11),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.position="top") +
  xlab("Modelo") + ylab ("Tamanho de efeito padronizado")
s


# probabilities of detection
det_res <- as.data.frame (res_occ_models$NoSpace$summary[grep ("muP",rownames(res_occ_models$NoSpace$summary)),])
det_res <- cbind (det_res, base=c("GBIF", "eBird", "INat", "Vertnet", "Wikiaves"))
colnames(det_res)[c(3,7)] <- c("lower", "upper")
#
pd=position_dodge(0.5)

p1 <- ggplot (det_res ,#[which(DF_coef$Neighborhood %in% c("No","10km","75km")),],  
              aes (y=mean, x=base,
                   group = NULL,
                   colour=base))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.5,position=pd,size=1) +
  geom_line(position=pd) +
  geom_point(position=pd, aes(colour=base),size=2.5)  

p1# <-p1 + scale_color_manual(values=c("#A0A0A0", "#000000","#dfc27d","#80cdc1"))

r <- p1 + scale_y_continuous(limits=c(0,0.5),breaks=seq(0,0.5,0.1)) + 
  #coord_cartesian(ylim=c(-1.1,1.1)) +
  theme(plot.margin=unit(c(0,0.5,0,0),"lines"))
r
s <-r +  theme_classic()  + #coord_flip(ylim=c(-1,5))+
  scale_x_discrete()+
  theme(axis.text.x = element_text(angle=45,
                                   hjust = 1,
                                   size=11),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=13),
        axis.title.x = element_text(size=13),
        legend.position="none") +
  xlab("Database") + ylab ("Detection probability (average, CI)")
s

# mapa do best ranked model
## neighborhood of 10 km

load(here ("output","sampled.RData"))
load(here("data","organized_data", "INPUT_ebird.RData")) ## ebird

# dados para reodenar os municipios apos output
treino <- which(ID_MUN$ID_MUN %in% sampled$ID_MUN == F) 
teste <- which(ID_MUN$ID_MUN %in% sampled$ID_MUN == T) 
teste_ordem <- c(treino,teste)

# data frame, reodenadno os municipios
cores_10km <- data.frame (cores= res_occ_models$spatial10$mean$z [order(teste_ordem, decreasing=F)] ,
                     NM_MUNICIP=shape_RS$NM_MUNICIP)

# fortify
require(ggplot2)
f.mun<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_10km<- cbind (f.mun, 
               Nespecies= cores_10km [match (f.mun$id, cores_10km$NM_MUNICIP),]$cores)

## colocar o shape da america do sul = comum a todos
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## inserir os lagos = comum a todos
b <- a + geom_polygon (data=lagos,aes(x=long, y=lat, group=group), 
                                  fill="lightcyan",colour = "lightcyan", size=1)

## inserir estimativas
c_10km <-   b + geom_polygon(data=f.mun_10km, aes(x=long, y=lat, group=group, 
                                        color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "10 km cell size")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_10km$cores)), 
                        breaks=seq(0,max(cores_10km$cores,na.rm=T),by=0.2),
                        name="Real occupancy state (zi)") ## para continuo
  

d_10km <- c_10km + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.3)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.3)

e_10km <- d_10km + ggsn::scalebar(f.mun_10km, dist = 100, st.dist=0.03,st.size=2.5, height=0.04, 
                      transform = TRUE, dist_unit = "km",
                      model = 'WGS84', location = "bottomright")

## plot para retirar a legenda

f_10km_legend <- e_10km +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),

        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.key.width=unit(0.85,"cm"),
        legend.key.size = unit(0.40,"cm"),
        legend.position = "top",
        legend.justification = 0.5,
        legend.direction="horizontal",
        legend.box="horizontal",
        axis.text=element_text(size=3),
        axis.text.x = element_text(size=3),
        axis.title.x = element_text(size = 5),
        axis.text.y = element_text(size=3),
        axis.title.y = element_text(size = 5),
        plot.title = element_text(size=8),
        plot.margin = unit(c(-0.1, -0.1,-0.1, -0.2), "lines")) 
#        legend.margin = margin (0,0,0,0),
 #       legend.box.margin = margin(1,0,0,0)) 

## extrair legenda
require(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legenda_comum <- get_legend(f_10km_legend) ## tem que primeiro gerar um mapa com legenda

## plot para o painel (sem a legenda)  
f_10km <- e_10km + 
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key.width=unit(0.4,"cm"),
        legend.key.size = unit(0.5,"cm"),
        axis.text=element_text(size=5),
        axis.text.x = element_text(size=5),
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size=5),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(0.2,-2, -3.5, -1.0), "lines"))
# top, right, bottom, and left margins

f_north_10km <- f_10km + ggsn::north(f.mun_10km, symbol=1,scale = 0.2,location = "bottomleft") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8))


# arranjo com campo e espaço
# data frame, reodenadno os municipios
cores_10km_grass <- data.frame (cores= res_occ_models$grass_aut10$mean$z [order(teste_ordem, decreasing=F)] ,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)
# fortify
f.mun_10km_grass<- cbind (f.mun, 
                    Nespecies= cores_10km_grass [match (f.mun$id, cores_10km_grass$NM_MUNICIP),]$cores)

## inserir estimativas
c_10km_grass <-   b + geom_polygon(data=f.mun_10km_grass, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "10 km cell size")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_10km$cores)), 
                        breaks=seq(0,max(cores_10km$cores,na.rm=T),by=0.2),
                        name="Real occupancy state (zi)") ## para continuo


d_10km <- c_10km + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.3)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.3)

e_10km <- d_10km + ggsn::scalebar(f.mun_10km, dist = 100, st.dist=0.03,st.size=2.5, height=0.04, 
                                  transform = TRUE, dist_unit = "km",
                                  model = 'WGS84', location = "bottomright")

## plot para retirar a legenda

f_10km_legend <- e_10km +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        legend.key.width=unit(0.85,"cm"),
        legend.key.size = unit(0.40,"cm"),
        legend.position = "top",
        legend.justification = 0.5,
        legend.direction="horizontal",
        legend.box="horizontal",
        axis.text=element_text(size=3),
        axis.text.x = element_text(size=3),
        axis.title.x = element_text(size = 5),
        axis.text.y = element_text(size=3),
        axis.title.y = element_text(size = 5),
        plot.title = element_text(size=8),
        plot.margin = unit(c(-0.1, -0.1,-0.1, -0.2), "lines")) 
#        legend.margin = margin (0,0,0,0),
#       legend.box.margin = margin(1,0,0,0)) 

f_10km_legend

## arranjo 10 km e sem espaço

pdf(file="maps_estimates_NoSp_10km.pdf",width = 7,height = 5,family="serif")

grid.arrange(f_sem,
             f_north_10km,
             legenda_comum,
             ncol=11,nrow=4,
             layout_matrix = rbind(c(NA,NA,3,3,3,3,3,3,3,NA,NA), 
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA),
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA),
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA))) 


dev.off()


## mapa da vizinhanca de 25 km

cores_25km <- data.frame (cores= res_occ_models$d25kmCellSize$mean$z,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

f.mun_25km<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_25km<- cbind (f.mun_25km, 
                    Nespecies= cores_25km [match (f.mun_25km$id, cores_25km$NM_MUNICIP),]$cores)


## inserir estimativas
c_25km <-   b + geom_polygon(data=f.mun_25km, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "25 km cell size")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_25km$cores)), 
                        breaks=seq(0,max(cores_25km$cores,na.rm=T),by=0.2),
                        name="Probability") ## para continuo


f_25km <- c_25km + 
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2,-2, -3.5, -1.0), "lines"))

f_25km

## vizinhanca de 50 km

cores_50km <- data.frame (cores= res_occ_models$d50kmCellSize$mean$z,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

f.mun_50km<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_50km<- cbind (f.mun_50km, 
                    Nespecies= cores_50km [match (f.mun_50km$id, cores_50km$NM_MUNICIP),]$cores)


## inserir estimativas
c_50km <-   b + geom_polygon(data=f.mun_50km, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "50 km cell size")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_50km$cores)), 
                        breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Probability") ## para continuo

# 
f_50km <- c_50km + 
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2,-2, -3.5, -1.0), "lines"))

f_50km


#library("gridExtra")
#png("arranjo_estimativas_Z.png", width=16,height=15,units="cm", res=600,family="serif")

pdf(file=here ("output","maps_estimates"),width = 7,height = 5,family="serif")

grid.arrange(f_sem,
             f_north_10km, 
             f_25km,
             f_50km,
             #f_75km,
             legenda_comum,
             ncol=6,nrow=10,
             layout_matrix = rbind(c(1,1,1,2,2,2), 
                                   c(1,1,1,2,2,2), 
                                   c(1,1,1,2,2,2), 
                                   rep(NA,6),
                                   c(3,3,3,4,4,4), 
                                   c(3,3,3,4,4,4),
                                   c(3,3,3,4,4,4),
                                   rep(NA,6),
                                   c(5,5,5,5,5,5),
                                   c(5,5,5,5,5,5)
             )) 


dev.off()


### MAPAS DAS OBSERVACOES
require(ggplot2)
## colocar o shape da america do sul = comum a todos
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## inserir os lagos = comum a todos
b <- a + geom_polygon (data=lagos,aes(x=long, y=lat, group=group), 
                       fill="lightcyan",colour = "lightcyan", size=1)

## abrir deteccoes do ebird

load (here("data","deteccoes", "ebird", "INPUT_ebird.RData"))## gbif

ebird_rhea <- apply (df_mun_rhea, 1, max,na.rm=T)

## mapas das deteccoes
cores_ebird <- data.frame (cores= ebird_rhea,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_ebird$cores <- factor(cores_ebird$cores)
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == 0)] <- "Not detected"
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == 1)] <- "Detected"
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == -Inf)] <- "Not sampled"

# fortify
f.mun_ebird<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_ebird<- cbind (f.mun_ebird, 
                    Nespecies= cores_ebird [match (f.mun_ebird$id, 
                                                  cores_ebird$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_ebird <-   b + geom_polygon(data=f.mun_ebird, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                               colour=NA,size=1) + 
  labs (title= "eBird") +
  scale_fill_manual("Observation data",
                     values = c("Not detected" = "white",
                                "Detected" = "darkred",
                                "Not sampled" = "gray85"))
  

d_ebird <- c_ebird + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.1) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.1)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.1)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.1)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.1)

e_ebird <- d_ebird + ggsn::scalebar(f.mun_ebird, dist = 100, st.dist=0.03,st.size=2.2, height=0.02, 
                                  transform = TRUE, dist_unit = "km",
                                  model = 'WGS84', location = "bottomright")

## plot para retirar a legenda

f_ebird_legend <- e_ebird +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        
        legend.title = element_text(size=9),
        legend.text = element_text(size=7),
        legend.key.width=unit(0.85,"cm"),
        legend.key.size = unit(0.40,"cm"),
        legend.position = "top",
        legend.justification = 0.5,
        legend.direction="horizontal",
        legend.box="horizontal",
        axis.text=element_text(size=3),
        axis.text.x = element_text(size=3),
        axis.title.x = element_text(size = 5),
        axis.text.y = element_text(size=3),
        axis.title.y = element_text(size = 5),
        plot.title = element_text(size=8),
        plot.margin = unit(c(0.1, -0.1,-0.1, 0.2), "lines")) 
#        legend.margin = margin (0,0,0,0),
#       legend.box.margin = margin(1,0,0,0)) +

f_ebird_legend

## extrair legenda
require(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legenda_comum_data <- get_legend(f_ebird_legend) ## tem que primeiro gerar um mapa com legenda

## plot para o painel (sem a legenda)  
f_ebird <- e_ebird + 
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key.width=unit(0.4,"cm"),
        legend.key.size = unit(0.5,"cm"),
        axis.text=element_text(size=5),
        axis.text.x = element_text(size=5),
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size=5),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(-0.1,-0.1, -3.9, 0.05), "lines"))
# top, right, bottom, and left margins


f_north_ebird <- f_ebird + ggsn::north(f.mun_ebird, symbol=1,scale = 0.2,location = "bottomleft") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8))

f_north_ebird

## abrir deteccoes GIBF
load (here("data","deteccoes", "Gbif", "input_GBIF.RData"))## gbif

## mapas das deteccoes
cores_gbif <- data.frame (cores= dados_det_ema_gbif$det,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_gbif$cores [which (dados_det_ema_gbif$riqueza_aves == 0)] <- NA

require(ggplot2)
f.mun_gbif<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_gbif<- cbind (f.mun_gbif, 
                    Nespecies= cores_gbif [match (f.mun_gbif$id, 
                                                  cores_gbif$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_gbif <-   b + geom_polygon(data=f.mun_gbif, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "GBIF") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo


f_gbif <- c_gbif + 
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2,-0.1, -3, -0.1), "lines"))
# top, right, bottom, and left margins


## abrir deteccoes wikiaves
load (here("data","deteccoes", "wikiaves", "input_wikiaves.RData"))## gbif

## mapas das deteccoes
cores_wiki <- data.frame (cores= dados_wikiaves$RHAMERICANA,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_wiki$cores [which (dados_wikiaves$NSPECIES == 0)] <- NA

require(ggplot2)
f.mun_wiki <-fortify(shape_RS, region="NM_MUNICIP")
f.mun_wiki <- cbind (f.mun_wiki, 
                    Nespecies= cores_wiki [match (f.mun_wiki$id, 
                                                  cores_wiki$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_wiki <-   b + geom_polygon(data=f.mun_wiki, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Wikiaves") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo

f_wiki <- c_wiki +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.2,0, -3, -0.1), "lines"))
# top, right, bottom, and left margins

f_wiki

## abrir deteccoes INAT
load (here("data","deteccoes", "outras_bases_INAT", "input_INAT.RData"))## gbif

## mapas das deteccoes
cores_inat <- data.frame (cores= dados_det_ema_inat$det,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_inat$cores [which (dados_det_ema_inat$riqueza_aves == 0)] <- NA

require(ggplot2)
f.mun_inat <-fortify(shape_RS, region="NM_MUNICIP")
f.mun_inat <- cbind (f.mun_inat, 
                     Nespecies= cores_inat [match (f.mun_inat$id, 
                                                   cores_inat$NM_MUNICIP),]$cores)

## inserir deteccoes do INAT
c_inat <-   b + geom_polygon(data=f.mun_inat, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "INaturalist") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo
f_inat<- c_inat +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-3.7,-0.1, 0.2, 0), "lines"))
# top, right, bottom, and left margins

## carregar vertnet
load (here("data","deteccoes", "vertnet", "input_VERTNET.RData"))## gbif

## mapas das deteccoes
cores_vertnet <- data.frame (cores= dados_det_ema_vertnet$det,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_vertnet$cores [which (dados_det_ema_vertnet$riqueza_aves == 0)] <- NA

require(ggplot2)
f.mun_vertnet <-fortify(shape_RS, region="NM_MUNICIP")
f.mun_vertnet <- cbind (f.mun_vertnet, 
                     Nespecies= cores_vertnet [match (f.mun_vertnet$id, 
                                                   cores_vertnet$NM_MUNICIP),]$cores)

## inserir deteccoes do INAT
c_vertnet <-   b + geom_polygon(data=f.mun_vertnet, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Vertnet") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo

f_vertnet <- c_vertnet + 
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,-0.1, 0, -0.1), "lines"))
# top, right, bottom, and left margins
## concenso entre as bases

cores_concenso <- cbind(apply (df_mun_rhea, 1, max,na.rm=T), 
                  cores_gbif$cores, 
                  cores_inat$cores,
                  cores_vertnet$cores,
                  cores_wiki$cores)
## numero de deteccoes por base
apply(cores_concenso, 2, function (i) sum (i > 0,na.rm=T))
sum(apply (df_mun_rhea, 1, max,na.rm=T)>0)
apply (cores_concenso,2,function (i) sum(i>0,na.rm=T))

cores_concenso <- data.frame (cores= apply (cores_concenso,1,max,na.rm=T),
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

## numero de municipios com observacao concenso
sum(cores_concenso$cores>0,na.rm=T)

require(ggplot2)
f.mun_con <-fortify(shape_RS, region="NM_MUNICIP")
f.mun_con <- cbind (f.mun_con, 
                     Nespecies= cores_concenso [match (f.mun_con$id, 
                                                       cores_concenso$NM_MUNICIP),]$cores)

## inserir deteccoes do INAT
c_con <-   b + geom_polygon(data=f.mun_con, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Concensus among databases") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo
f_con<- c_con +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-5,0, 0, -0.1), "lines"))

# top, right, bottom, and left margins

######### arranjar o painel
pdf(file=here ("output","maps_observation"),width = 7,height = 5,family="serif")

grid.arrange(f_north_ebird, f_gbif,f_wiki,
             legenda_comum_data,
             f_inat,f_vertnet,f_con,
             ncol=3,nrow=3,
             layout_matrix = cbind(c(1,4,5), 
                                   c(2,4,6),
                                   c(3,4,7))) 
dev.off()

#### MAPAS DOS EXPERTS

files_expert <- list.files(here ("data", "expert_opinion"),pattern=".rds")

## expert data
expert_data <- lapply (files_expert, function (i)
  readRDS (here ("data", "expert_opinion", i)))

# over each expert opinion on RS shape
crs(shape_RS)<-crs (expert_data[[1]]$Shape_Mun)
expert_rhea<-lapply (expert_data, function (i) 
  over (shape_RS,i$Shape_Mun))
# melt the list
expert_rhea<- do.call(cbind,expert_rhea)
# adjusting
expert_rhea<- ifelse(is.na(expert_rhea),0,1)

# summing and getting presence
expert_rhea_presence <- ifelse (rowSums (expert_rhea)>1,1,0)

## osbrepor o expert com mapa do RS
cores_expert <- data.frame (cores= expert_rhea_presence,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)
sum(cores_expert$cores)

require(ggplot2)
f.mun_expert <-fortify(shape_RS, region="NM_MUNICIP")
f.mun_expert <- cbind (f.mun_expert, 
                     Nespecies= cores_expert [match (f.mun_expert$id, 
                                                   cores_expert$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_expert <-   b + geom_polygon(data=f.mun_expert, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Expert knowledge") +
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "gray85",
                        limits=c(0,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Detected") ## para continuo

f_expert <- c_expert +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,0, -0.5, 0), "lines"))
# top, right, bottom, and left margins

f_expert

## ARRANJO DAS OBSERVACOES, EXPERTS, E ESTIMATIVAS

## estimates
## inserir estimativas
c_sem <-   b + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Species distribution modeling")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_sem$cores)), 
                        breaks=seq(0,max(cores_sem$cores,na.rm=T),by=0.2),
                        name="Probability") ## para continuo


d_sem <- c_sem + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.3)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.3)

e_sem <- d_sem + ggsn::scalebar(f.mun_sem, dist = 100, st.dist=0.03,st.size=2.5, height=0.04, 
                                  transform = TRUE, dist_unit = "km",
                                  model = 'WGS84', location = "bottomright")

f_sem <- e_sem + 
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key.width=unit(0.4,"cm"),
        legend.key.size = unit(0.5,"cm"),
        axis.text=element_text(size=5),
        axis.text.x = element_text(size=5),
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size=5),
        axis.title.y = element_text(size = 8),
        plot.margin = unit(c(-3,-1,0,0), "lines"))
# top, right, bottom, and left margins


f_north_sem <- f_sem + ggsn::north(f.mun_sem, symbol=1,scale = 0.2,location = "bottomleft") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8))

f_north_sem

## concensus DB
f_con<- c_con +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-5,0, -1, 0), "lines"))

# top, right, bottom, and left margins

## plot somente para extrair a legenda
f_ebird_legend <- e_ebird +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        
        legend.title = element_text(size=9),
        legend.text = element_text(size=7),
        legend.key.width=unit(0.85,"cm"),
        legend.key.size = unit(0.40,"cm"),
        legend.position = "top",
        legend.justification = 0.5,
        legend.direction="vertical",
        legend.box="horizontal",
        axis.text=element_text(size=3),
        axis.text.x = element_text(size=3),
        axis.title.x = element_text(size = 5),
        axis.text.y = element_text(size=3),
        axis.title.y = element_text(size = 5),
        plot.title = element_text(size=8),
        plot.margin = unit(c(-2, -2,-2, -2), "lines")) 
#        legend.margin = margin (0,0,0,0),
#       legend.box.margin = margin(1,0,0,0)) +

# f_ebird_legend

## extrair legenda
require(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legenda_comum_data <- get_legend(f_ebird_legend) ## tem que primeiro gerar um mapa com legenda

pdf(file=here ("output","arranjo_expert_estimate_obs"),width = 7,height = 6,family="serif")

grid.arrange(f_expert,
             f_con,
             f_north_sem,
             legenda_comum_data,
             legenda_comum,
             ncol=3,nrow=3,
             layout_matrix = rbind(c(4,5,5),
                                   c(1,3,3),
                                   c(2,3,3)))
dev.off()

# arranjo do modelo, experts, e diferenca

f.mun_difference <- f.mun_expert
f.mun_difference$Nespecies <- f.mun_expert$Nespecies - f.mun_sem$Nespecies

## inserir deteccoes do GBIF
c_diff <-   b + geom_polygon(data=f.mun_difference, aes(x=long, y=lat, group=group, 
                                                      color=Nespecies, fill=Nespecies), 
                               colour = NA, size=1) + 
  labs (title= "Expert knowledge (blue) minus SDM (red)") +
  scale_fill_gradient2 (low='red',mid="white", 
                        high='blue', 
                        midpoint= 0,na.value = "gray85",
                        limits=c(-1,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Difference") ## para continuo

f_expert <- c_expert +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,0, -0.5, 0), "lines"))
# top, right, bottom, and left margins

f_expert

grid.arrange(f_expert,
             f_con,
             f_north_sem,
             legenda_comum_data,
             legenda_comum,
             ncol=3,nrow=3,
             layout_matrix = rbind(c(4,5,5),
                                   c(1,3,3),
                                   c(2,3,3)))

