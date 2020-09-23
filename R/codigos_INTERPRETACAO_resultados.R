## mapinha pra ver qual é!

## pacote para readOGR
require(rgdal)
require(raster)
require(here)
shape_RS <- readOGR(dsn=here("covariaveis","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## obter os lagos para pintar diferente depois
lagos <- shape_RS [c(96,250),]
## remover os lagos
shape_RS <- shape_RS [-c(96,250),]

## shape south america
southAme<- readOGR(dsn=here ("South_America"),encoding="latin1", layer="South_America")
BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(BR_AR_URU)<-crs(shape_RS)

## carregar os resultados dos modelos
load (here ("RESULTADOS", "out_sem_espaco.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_10km.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_25km.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_50km.RData"))

## mapas da ocupação estimada

## sem espaco

cores_sem <- data.frame (cores= out_sem_espaco_sem_gado$mean$z,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)
sum(out_sem_espaco_sem_gado$mean$z)
mean(out_sem_espaco_sem_gado$sd$z)


require(ggplot2)
f.mun_sem<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_sem<- cbind (f.mun_sem, 
                    Nespecies= cores_sem [match (f.mun_sem$id, cores_sem$NM_MUNICIP),]$cores)

## colocar o shape da america do sul = comum a todos
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## inserir os lagos = comum a todos
b <- a + geom_polygon (data=lagos,aes(x=long, y=lat, group=group), 
                       fill="lightcyan",colour = "lightcyan", size=1)

## inserir estimativas
c_sem <-   b + geom_polygon(data=f.mun_sem, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "No spatial neighborhood")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_sem$cores)), 
                        breaks=seq(0,max(cores_sem$cores,na.rm=T),by=0.2),
                        name="Probability") ## para continuo

f_sem <- c_sem + 
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

## neighborhood of 10 km

cores_10km <- data.frame (cores= out_espaco_10km_sem_gado$mean$z,
                     NM_MUNICIP=shape_RS$NM_MUNICIP)

require(ggplot2)
f.mun_10km<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_10km<- cbind (f.mun_10km, 
               Nespecies= cores_10km [match (f.mun_10km$id, cores_10km$NM_MUNICIP),]$cores)

## colocar o shape da america do sul = comum a todos
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## inserir os lagos = comum a todos
b <- a + geom_polygon (data=lagos,aes(x=long, y=lat, group=group), 
                                  fill="lightcyan",colour = "lightcyan", size=1)

## inserir estimativas
c_10km <-   b + geom_polygon(data=f.mun_10km, aes(x=long, y=lat, group=group, 
                                        color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Neighborhood of 10 km")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_10km$cores)), 
                        breaks=seq(0,max(cores_10km$cores,na.rm=T),by=0.2),
                        name="Real occupancy state (z^i)") ## para continuo
  

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
        plot.margin = unit(c(0.1, -0.1,-0.1, 0.2), "lines")) 
#        legend.margin = margin (0,0,0,0),
 #       legend.box.margin = margin(1,0,0,0)) 

f_10km_legend

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

## arranjo 10 km e sem espaço

pdf(file="maps_estimates_NoSp_10km.pdf",width = 7,height = 5,family="serif")

grid.arrange(f_north_10km,
             f_sem,
             legenda_comum,
             ncol=11,nrow=4,
             layout_matrix = rbind(c(NA,NA,3,3,3,3,3,3,3,NA,NA), 
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA),
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA),
                                   c(NA,1,1,1,1,NA,2,2,2,2,NA))) 


dev.off()


## mapa da vizinhanca de 25 km

cores_25km <- data.frame (cores= out_espaco_25km_sem_gado$mean$z,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

f.mun_25km<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_25km<- cbind (f.mun_25km, 
                    Nespecies= cores_25km [match (f.mun_25km$id, cores_25km$NM_MUNICIP),]$cores)


## inserir estimativas
c_25km <-   b + geom_polygon(data=f.mun_25km, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Neighborhood of 25 km")+
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
        plot.margin = unit(c(0.2,-1, -3, -3), "lines"))

f_25km

## vizinhanca de 50 km

cores_50km <- data.frame (cores= out_espaco_50km_sem_gado$mean$z,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

f.mun_50km<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_50km<- cbind (f.mun_50km, 
                    Nespecies= cores_50km [match (f.mun_50km$id, cores_50km$NM_MUNICIP),]$cores)


## inserir estimativas
c_50km <-   b + geom_polygon(data=f.mun_50km, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Neighborhood of 50 km")+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.20,na.value = "white",
                        limits=c(0,max(cores_50km$cores)), 
                        breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Probability") ## para continuo


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
        plot.margin = unit(c(-2.5,-2, -0.5, 0.2), "lines"))

f_50km


#library("gridExtra")
#png("arranjo_estimativas_Z.png", width=16,height=15,units="cm", res=600,family="serif")

pdf(file="maps_estimates.pdf",width = 7,height = 5,family="serif")

grid.arrange(f_north_10km, f_25km,
             f_50km,#f_75km,
             legenda_comum,
             ncol=2,nrow=3,
             layout_matrix = cbind(c(1,5,3), 
                                   c(2,5,4))) 


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

load (here("deteccoes", "ebird", "INPUT_ebird.RData"))## gbif

ebird_rhea <- apply (df_mun_rhea, 1, max,na.rm=T)

## mapas das deteccoes
cores_ebird <- data.frame (cores= ebird_rhea,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

cores_ebird$cores <- factor(cores_ebird$cores)
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == 0)] <- "Non-detected"
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == 1)] <- "Detected"
levels(cores_ebird$cores) [which(levels (cores_ebird$cores) == -Inf)] <- "Not sampled"


f.mun_ebird<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_ebird<- cbind (f.mun_ebird, 
                    Nespecies= cores_ebird [match (f.mun_ebird$id, 
                                                  cores_ebird$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_ebird <-   b + geom_polygon(data=f.mun_ebird, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                               colour=NA,size=1) + 
  labs (title= "Ebird") +
  scale_fill_manual("Observation data",
                     values = c("Non-detected" = "white",
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
load (here("deteccoes", "Gbif", "input_GBIF.RData"))## gbif

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
load (here("deteccoes", "wikiaves", "input_wikiaves.RData"))## gbif

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
load (here("deteccoes", "outras_bases_INAT", "input_INAT.RData"))## gbif

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
load (here("deteccoes", "vertnet", "input_VERTNET.RData"))## gbif

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
pdf(file="maps_observation.pdf",width = 7,height = 5,family="serif")

grid.arrange(f_north_ebird, f_gbif,f_wiki,
             legenda_comum_data,
             f_inat,f_vertnet,f_con,
             ncol=3,nrow=3,
             layout_matrix = cbind(c(1,4,5), 
                                   c(2,4,6),
                                   c(3,4,7))) 
dev.off()

#### MAPAS DOS EXPERTS

expert_rhea <- readOGR(dsn=here("EXPERTS"), layer="arquivo_poligono_Rhea",
        encoding = "UTF-8",use_iconv = T)
crs (expert_rhea) <- crs (shape_RS)

## osbrepor o expert com mapa do RS
cores_expert <- data.frame (cores= ifelse (is.na(over (shape_RS, expert_rhea)$nm_plgn), 0,1),
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
  labs (title= "Drunk expert") +
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

pdf(file="arranjo_expert_estimate_obs.pdf",width = 7,height = 6,family="serif")

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

### arranjo de sem espaço e 10 km
f_north_sem



########################################
## interpretação dos coeficientes
########################################

## carregar os resultados dos modelos
load (here ("RESULTADOS", "Rhea_out_sem_espaco_com_floresta.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_10km_com_floresta.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_25km_com_floresta.RData"))
load (here ("RESULTADOS", "Rhea_CAR_espaco_50km_com_floresta.RData"))
load (here ("RESULTADOS", "Rhea_out_espaco_75km_com_floresta.RData"))

# sem espaco
sem_espaco <- cbind(data.frame(Neighborhood="No"),
  out_sem_espaco_com_floresta$summary [grep("BETA", rownames(out_sem_espaco_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")]
)

# 10 km

dez_km <- cbind(data.frame(Neighborhood="10km"),
      rbind (
  out_espaco_10km_com_floresta$summary [grep("BETA", rownames(out_espaco_10km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")],
  out_espaco_10km_com_floresta$summary [grep("spacesigma", rownames(out_espaco_10km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")]
)
)

# 25km 
vintecinco_km <- cbind(data.frame(Neighborhood="25km"),
      
      rbind(
        
        out_espaco_25km_com_floresta$summary [grep("BETA", rownames(out_espaco_25km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")],
        out_espaco_25km_com_floresta$summary [grep("spacesigma", rownames(out_espaco_25km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")]
  )
)
# 50 km
cinquenta_km <- cbind(data.frame(Neighborhood="50km"),
      rbind(
          
          out_floresta$summary [grep("BETA", rownames(out_floresta$summary)),c("mean", "sd","2.5%", "97.5%")],
          out_floresta$summary [grep("spacesigma", rownames(out_floresta$summary)),c("mean", "sd","2.5%", "97.5%")]
  )
)

# 75km 
setenta_e_cinco_km <- cbind(data.frame(Neighborhood="75km"),
      
      rbind (
      
          out_espaco_75km_com_floresta$summary [grep("BETA", rownames(out_espaco_75km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")],
          out_espaco_75km_com_floresta$summary [grep("spacesigma", rownames(out_espaco_75km_com_floresta$summary)),c("mean", "sd","2.5%", "97.5%")]
    )
)

dez_km$par <- c(rownames(sem_espaco),"SpaceSigma")
vintecinco_km$par <- c(rownames(sem_espaco),"SpaceSigma")
cinquenta_km$par <- c(rownames(sem_espaco),"SpaceSigma")
setenta_e_cinco_km$par <- c(rownames(sem_espaco),"SpaceSigma")
sem_espaco$par <- rownames(sem_espaco)

## juntar os DFs
DF_coef <- rbind(sem_espaco,
                 dez_km,
                 vintecinco_km,
                 cinquenta_km)#,
                 #setenta_e_cinco_km)
colnames(DF_coef)[4:5] <- c("lower","upper")

DF_coef$par [which(DF_coef$par == "BETA0")] <- "Intercept"
DF_coef$par [which(DF_coef$par == "SpaceSigma")] <- "CARSigma"
DF_coef$par <- gsub ("BETA.","",DF_coef$par)
##
DF_coef$par [which(DF_coef$par == "BOVINO")] <- "Cattle"
DF_coef$par [which(DF_coef$par == "OVELHA")] <- "Sheep"
DF_coef$par [which(DF_coef$par == "FLORESTA")] <- "Forest"
DF_coef$par [which(DF_coef$par == "CAMPO")] <- "Grassland"
DF_coef$par [which(DF_coef$par == "SOJA")] <- "Soybean"
DF_coef$par<- factor (DF_coef$par, 
                      levels = c("Intercept",
                                 "Soybean",
                                 "Cattle",
                                 "Sheep",
                                 "Forest",
                                 "Grassland",
                                 "CARSigma"))


## plot
require(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

pd=position_dodge(0.5)

p1 <- ggplot (DF_coef ,#[which(DF_coef$Neighborhood %in% c("No","10km","75km")),],  
             aes (y=mean, x=par, fill=Neighborhood,
                  group = NULL,
                  colour=Neighborhood))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.5,position=pd,size=1) +
  geom_line(position=pd) +
  geom_point(position=pd, aes(colour=Neighborhood),size=2.5)

p1 <-p1 + scale_color_manual(values=c("#A0A0A0", "#000000","#dfc27d","#80cdc1"))

r <- p1 + scale_y_continuous(limits=c(-2.2,5.5),breaks=-2:5.5) + 
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
  xlab("Covariates") + ylab ("Standardized effect size")
s

## dados para tabela 1

cbind(
  round(rbind(sum(out_sem_espaco_com_floresta$mean$z),
    mean(out_sem_espaco_com_floresta$sd$z)),2),

  round(rbind(sum(out_espaco_10km_com_floresta$mean$z),
    mean(out_espaco_10km_com_floresta$sd$z)),2),

  round(rbind(sum(out_espaco_25km_com_floresta$mean$z),
    mean(out_espaco_25km_com_floresta$sd$z)),2),

  round(rbind(sum(out_floresta$mean$z),
    mean(out_floresta$sd$z)),2),

  round(rbind(sum(out_espaco_75km_com_floresta$mean$z),
    mean(out_espaco_75km_com_floresta$sd$z)),2))
