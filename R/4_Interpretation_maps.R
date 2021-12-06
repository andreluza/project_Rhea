# -----------------
## mapping & analysis

# load packages
source ("R/packages.R")

# shapefile RS 
shape_RS <- readOGR(dsn=here("data","shape_munRS"), 
                    layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## lakes (will use them when mapping)
lagos <- shape_RS [c(96,250),]
## remove lakes
shape_RS <- shape_RS [-c(96,250),]

## shape south america
southAme<- readOGR(dsn=here ("data","South_America"),encoding="latin1", layer="South_America")
BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(BR_AR_URU)<-crs(shape_RS)

## load Pampa shapefile useful for maps and other stuff
pampa <- readOGR(dsn=here("data","pampa_border"), 
                    layer="biome_border",
                    encoding = "UTF-8",use_iconv = T)
crs(pampa)<-crs(shape_RS)

## load models
list.res <- list.files (here("output"), pattern = "out_*")

load(here("output", "out_null_model1.RData"))
load(here("output", "out_model2.RData"))
load(here("output", "out_grass_model3.RData"))
load(here("output", "out_grass_agri_model4.RData"))
load(here("output", "out_model5.RData"))
load(here("output", "out_complete_model6.RData"))

# load validation data (for model selection)
load(here("output", "validation_data.RData"))
n_so_ebird<-11
#list modeels
res_occ_models <- list(null=out_null_model1,
                   spatial10=out_model2[[1]],
                   spatial25=out_model2[[2]],
                   spatial50=out_model2[[3]],
                   grass_none=out_grass_model3,
                   complete_none=out_grass_agri_model4,
                   grass_aut10 = out_model5[[1]],
                   grass_aut25 = out_model5[[2]],
                   grass_aut50 = out_model5[[3]],
                   complete10 = out_complete_model6[[1]],
                   complete25 = out_complete_model6[[2]],
                   complete50 = out_complete_model6[[3]]
)
# df for model ranking
# loade deviance
load(here("output","deviance.RData"))

# estimates of number occupied municipalities (true occupancy)
z_per_model<-cbind (
  do.call (rbind, lapply (res_occ_models, function (i) 
    i$summary[grep("fs.z", rownames(i$summary)),])),
  # last column, sd z
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


#----------------------------------------- #
# map best ranked model
load(here ("output","sampled.RData"))
load(here("data","organized_data", "INPUT_ebird.RData")) ## ebird

# reordering of municipalities
treino <- which(ID_MUN$ID_MUN %in% sampled$ID_MUN == F) 
teste <- which(ID_MUN$ID_MUN %in% sampled$ID_MUN == T) 
teste_ordem <- c(treino,teste)

# data frame, reordered
cores_50km <- data.frame (cores= res_occ_models$spatial50$mean$z [order(teste_ordem, decreasing=F)] ,
                     NM_MUNICIP=shape_RS$NM_MUNICIP)

# fortify
require(ggplot2)
f.mun<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_50km<- cbind (f.mun, 
               Nespecies= cores_50km [match (f.mun$id, cores_50km$NM_MUNICIP),]$cores)

## plot south america shapefile
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## plot lakes
b <- a + geom_polygon (data=lagos,aes(x=long, y=lat, group=group), 
                                  fill="lightcyan",colour = "lightcyan", size=1)

## insert estimates
c_50km <-   b + geom_polygon(data=f.mun_50km, aes(x=long, y=lat, group=group, 
                                        color=Nespecies, fill=Nespecies), colour = NA, size=1) + 
  labs (title= "Species distribution modeling (SDM)")+
  scale_fill_gradient2 (low='white', high='darkred', 
                        midpoint= 0.2,
                        na.value = "white",
                        limits=c(0,max(cores_50km$cores)), 
                        breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name=expression("z" [i])) ## para continuo
  

d_50km <- c_50km + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.3)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.3)

e_50km <- d_50km + ggsn::scalebar(f.mun_50km, dist = 100, st.dist=0.03,st.size=2.5, height=0.04, 
                      transform = TRUE, dist_unit = "km",
                      model = 'WGS84', location = "bottomright")

## plot para retirar a legenda

f_50km_legend <- e_50km +
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

#pdf(file=here ("output","mapa_prob.pdf"),width = 4,height = 4,family="serif")

#f_50km_legend

#dev.off()

f_50km_legend <- f_50km_legend + geom_polygon (data=pampa,aes(x=long, y=lat, group=group), 
                              colour = "black", 
                              fill=NA,
                              size=1,
					linetype = "dashed")

# north
f_50km_legend <- f_50km_legend + ggsn::north(f.mun_50km, symbol=1,scale = 0.2,location = "bottomleft") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8))


# --------------------------------------------- #
#             MAP of EXPERTS

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

## over expert and RS maps
cores_expert <- data.frame (cores= expert_rhea_presence,
                            NM_MUNICIP=shape_RS$NM_MUNICIP)
sum(cores_expert$cores)

# fortify 
f.mun<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_expert <- cbind (f.mun, 
                       Nespecies= cores_expert [match (f.mun$id, 
                                                       cores_expert$NM_MUNICIP),]$cores)

## insert expert data
c_expert <-   b + geom_polygon(data=f.mun_expert, aes(x=long, y=lat, group=group, 
                                                      color=Nespecies, fill=Nespecies), 
                               colour = NA, size=1) + 
  labs (title= "Expert knowledge") +
  scale_fill_gradient2 (low='white', high='#284E78', midpoint= 0.20,na.value = "gray85",
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

#g_expert <- f_expert + annotate(geom="text", x=-56, y=-32, label="URUGUAI",color="black",size=2.1) +
#  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.1)+
#  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAI",color="black",size=2.1)+
#  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.1)+
#  annotate(geom="text", x=-50.5, y=-32.5, label="OCEANO ATL?NTICO",color="black",size=2.1)

#h_expert <- g_expert + ggsn::scalebar(f.mun_expert, dist = 100, st.dist=0.03,st.size=2.2, height=0.02, 
#                                      transform = TRUE, dist_unit = "km",
#                                      model = 'WGS84', location = "bottomright")
#
#i_expert <- h_expert + ggsn::north(f.mun_50km, symbol=1,scale = 0.2,location = "bottomleft") +
#  theme(legend.text=element_text(size=7),
#        legend.title=element_text(size=8))

i_expert<- f_expert + geom_polygon (data=pampa,aes(x=long, y=lat, group=group), 
                                    colour = "black", 
                                    fill=NA,
                                    size=1)

i_expert

# plot ber biome
pampa_mun <- over (shape_RS, pampa)
pampa_mun$ID <- ifelse (pampa_mun$ID ==1, "Pampa", "Atlantic Forest")
pampa_mun$ID[is.na(pampa_mun$ID)] <- "Atlantic Forest"
# colar z
pampa_mun$z <- cores_50km$cores
pampa_mun$data <- "SDM"

# expert data
expert <- pampa_mun
expert$z <- cores_expert$cores
expert$data <- "Expert"

# bind these data
pampa_mun<- rbind (pampa_mun,
                   expert)
pampa_mun[which(pampa_mun$ID == "Pampa" & pampa_mun$data == "Expert"), "z"]
pampa_mun[which(pampa_mun$ID == "Pampa"), "z"]

# difference
summary(aov(pampa_mun$z~pampa_mun$ID*pampa_mun$data))
summary(lm(pampa_mun$z~pampa_mun$ID*pampa_mun$data-1))

# violin
volin_biome <- ggplot (data = pampa_mun, aes (x=ID,
                                              y=z,
                                              fill=data,
                                              colour=data)) + 
  geom_violin(size=1) + 
  stat_summary(fun=mean,geom="point", shape=19, size=3) + 
  theme_classic() + xlab ("Biome") + 
	ylab (expression("Realized occurrence")) +
  theme (axis.title = element_text(size=15),
         axis.text = element_text(size=13))


pdf(file=here ("output","volin_biome.pdf"),width = 5,height = 5,family="serif")
volin_biome
dev.off()


# ----------------------------------------------------- #

# arranjo do modelo, experts, e diferenca

f.mun_difference <- f.mun_expert
f.mun_difference$Nespecies <- f.mun_expert$Nespecies - f.mun_50km$Nespecies

## inserir deteccoes do GBIF
c_diff <-   b + geom_polygon(data=f.mun_difference, aes(x=long, y=lat, group=group, 
                                                        color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Map of the difference") +
  scale_fill_gradient2 (low='darkred',mid="white", 
                        high='#284E78', 
                        midpoint= 0,na.value = "gray85",
                        limits=c(-1,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Expert - SDM") ## para continuo

f_diff <- c_diff +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,0, -0.5, 0), "lines"))
# top, right, bottom, and left margins

i_diff<-f_diff + geom_polygon (data=pampa,aes(x=long, y=lat, group=group), 
                               colour = "black", 
                               fill=NA,
                               size=1,
                               linetype = "dashed")

pdf(file=here ("output","arranjo_expert_estimate_diff.pdf"),width = 6,height = 6,family="serif")

grid.arrange(f_50km_legend,
             i_expert,
             #legenda_comum,
             i_diff,
             ncol=3,nrow=5,
             layout_matrix = rbind(#c(3,3,3),
                                   c(1,4,4),
                                   c(1,4,4),
                                   c(2,4,4),
                                   c(2,4,4)))

dev.off()

# -----------------------
# tabela de agreement

diferenca <- data.frame (exp=as.factor(cores_expert$cores),
                         sdm=cores_50km$cores,
                         diff=cores_expert$cores - cores_50km$cores)

t(data.frame ('Only expert' = table (diferenca$diff > 0)[2], # perito
              'Agreement betweeen expert and SDM' = table (diferenca$diff == 0)[2], # concordam
              'Only SDM' = table (diferenca$diff < 0)[2]) # perito
)


abs_diff <- ggplot (data = diferenca, aes (x=exp,
                               y=abs(diff),
                               fill=exp
                               )) + 
  geom_violin(size=1) + 
  stat_summary(fun=mean,geom="point", shape=19, size=3) + 
  theme_classic() + 
  xlab ("Occurrence based on the expert knowledge") + 
  ylab (expression("Absolute difference between expert and SDM")) +
  theme (axis.title = element_text(size=15),
         axis.text = element_text(size=13),
         legend.position = "none")

pdf(file=here ("output","abs_diff.pdf"),width = 5,height = 5,family="serif")
abs_diff
dev.off()

# 
summary(aov(abs(diferenca$diff)~diferenca$exp))


# ----------------------------------------
# MAPA da INCERTEZA
# -----------------------------------------

# data frame, reodenadno os municipios
cores_incerteza <- data.frame (cores= res_occ_models$spatial50$sd$psi [order(teste_ordem, decreasing=F)] ,
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

# fortify
f.mun<-fortify(shape_RS, region="NM_MUNICIP")
f.mun_incerteza<- cbind (f.mun, 
                    Nespecies= cores_incerteza [match (f.mun$id, cores_incerteza$NM_MUNICIP),]$cores)
f.mun_incerteza$Nespecies <- round(f.mun_incerteza$Nespecies ,3)

## inserir estimativas
c_incerteza <-   b + geom_polygon(data=f.mun_incerteza, aes(x=long, y=lat, group=group, 
                                                  color=round(Nespecies,2), 
								 fill=round(Nespecies,2)), 
									colour = NA, size=1) + 
  labs (title= expression(paste("Map of uncertainty on site-occupancy probability ", psi [i])))+
  scale_fill_gradient2 (low='white', high='darkred', midpoint= 0.15,na.value = "white",
                        limits=c(min(cores_incerteza$cores),max(cores_incerteza$cores)), 
                        breaks=seq(min(cores_incerteza$cores),max(cores_incerteza$cores,na.rm=T),
					by=0.05),
                        name= expression(paste("SD of ", psi [i])))


d_incerteza <- c_incerteza + annotate(geom="text", x=-56, y=-32, label="URUGUAY",color="black",size=2.3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAY",color="black",size=2.3)+
  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="ATLANTIC OCEAN",color="black",size=2.3)

e_incerteza <- d_incerteza + ggsn::scalebar(f.mun_incerteza, dist = 100, st.dist=0.03,st.size=2.5, height=0.04, 
                                  transform = TRUE, dist_unit = "km",
                                  model = 'WGS84', location = "bottomright")

## plot para retirar a legenda

f_incerteza <- e_incerteza +
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

f_incerteza<- f_incerteza + ggsn::north(f.mun_incerteza, symbol=1,scale = 0.2,location = "bottomleft") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8))

pdf(file=here ("output","mapa_incerteza.pdf"),width = 5,height = 5,family="serif")
f_incerteza
dev.off()


# -----------------------------------------
#         MAPAS DAS OBSERVACOES
# -----------------------------------------
require(ggplot2)
f.mun<-fortify(shape_RS, region="NM_MUNICIP")

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
f.mun_ebird<- cbind (f.mun, 
                    Nespecies= cores_ebird [match (f.mun$id, 
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

# fortify
f.mun_gbif<- cbind (f.mun, 
                    Nespecies= cores_gbif [match (f.mun$id, 
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

# forityf
f.mun_wiki <- cbind (f.mun, 
                    Nespecies= cores_wiki [match (f.mun$id, 
                                                  cores_wiki$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_wiki <-   b + geom_polygon(data=f.mun_wiki, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "WikiAves") +
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

# fortify
f.mun_inat <- cbind (f.mun, 
                     Nespecies= cores_inat [match (f.mun$id, 
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

# fority
f.mun_vertnet <- cbind (f.mun, 
                     Nespecies= cores_vertnet [match (f.mun$id, 
                                                   cores_vertnet$NM_MUNICIP),]$cores)

## inserir deteccoes do INAT
c_vertnet <-   b + geom_polygon(data=f.mun_vertnet, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "VertNet") +
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
#sum(apply (df_mun_rhea, 1, max,na.rm=T)>0)
# numero de municipios com dados coletados
apply(cores_concenso, 2, function (i) sum (i >= 0,na.rm=T))
#sum(apply (df_mun_rhea, 1, max,na.rm=T)>=0)

# cores
cores_concenso <- data.frame (cores= apply (cores_concenso,1,max,na.rm=T),
                          NM_MUNICIP=shape_RS$NM_MUNICIP)

## numero de municipios com observacao concenso
sum(cores_concenso$cores>0,na.rm=T)
# municipios na concenso sem amostra
table(is.infinite(cores_concenso$cores))

# fortify
f.mun_con <- cbind (f.mun, 
                     Nespecies= cores_concenso [match (f.mun$id, 
                                                       cores_concenso$NM_MUNICIP),]$cores)

## inserir deteccoes do INAT
c_con <-   b + geom_polygon(data=f.mun_con, aes(x=long, y=lat, group=group, 
                                                  color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Agregado das bases") +
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
pdf(file=here ("output","maps_observation.pdf"),width = 7,height = 5,family="serif")

grid.arrange(f_north_ebird, f_gbif,f_wiki,
             legenda_comum_data,
             f_inat,f_vertnet,f_con,
             ncol=3,nrow=3,
             layout_matrix = cbind(c(1,4,5), 
                                   c(2,4,6),
                                   c(3,4,7))) 
dev.off()


# -------------------------------------------------------
#   Experts vs incerteza

f.mun_difference_incerteza <- f.mun_expert
f.mun_difference_incerteza$Nespecies <- f.mun_expert$Nespecies - f.mun_incerteza$Nespecies

## inserir deteccoes do GBIF
c_diff_incerteza <-   b + geom_polygon(data=f.mun_difference_incerteza, aes(x=long, y=lat, group=group, 
                                                        color=Nespecies, fill=Nespecies), 
                             colour = NA, size=1) + 
  labs (title= "Mapa da diferença na incerteza") +
  scale_fill_gradient2 (low='darkred',mid="white", 
                        high='#284E78', 
                        midpoint= 0,na.value = "gray85",
                        limits=c(-0.5,1), 
                        #breaks=seq(0,max(cores_50km$cores,na.rm=T),by=0.2),
                        name="Diferença") ## para continuo

f_diff_incerteza <- c_diff_incerteza +
  xlab("") + ylab("") +
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
        plot.title = element_text(size=9),
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,0, -0.5, 0), "lines"))
# top, right, bottom, and left margins


pdf(file=here ("output","diff_incerteza.pdf"),width = 5,height = 5,family="serif")
f_diff_incerteza
dev.off()


# supporting information

# ------------------------------------------------------------------------------
# the other exertp map (Fig S1.1)
files_expert <- list.files(here ("data", "expert_opinion"),pattern=".rds")

## expert data
expert_data <- lapply (files_expert, function (i)
  readRDS (here ("data", "expert_opinion", i)))

# over each expert opinion on RS shape
crs(shape_RS)<-crs (expert_data[[1]]$Shape_campo)
expert_rhea<-lapply (expert_data, function (i) 
  over (shape_RS,i$Shape_campo))
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

# fortify 
f.mun_expert <- cbind (f.mun, 
                       Nespecies= cores_expert [match (f.mun$id, 
                                                       cores_expert$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_expert <-   b + geom_polygon(data=f.mun_expert, aes(x=long, y=lat, group=group, 
                                                      color=Nespecies, fill=Nespecies), 
                               colour = NA, size=1) + 
  labs (title= "Expert knowledge") +
  scale_fill_gradient2 (low='white', high='#284E78', midpoint= 0.20,na.value = "gray85",
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


#g_expert <- f_expert + annotate(geom="text", x=-56, y=-32, label="URUGUAI",color="black",size=2.1) +
#  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=2.1)+
#  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAI",color="black",size=2.1)+
#  annotate(geom="text", x=-51.8, y=-26.8, label="Santa Catarina",color="black",size=2.1)+
#  annotate(geom="text", x=-50.5, y=-32.5, label="OCEANO ATL?NTICO",color="black",size=2.1)

#h_expert <- g_expert + ggsn::scalebar(f.mun_expert, dist = 100, st.dist=0.03,st.size=2.2, height=0.02, 
#                                      transform = TRUE, dist_unit = "km",
#                                      model = 'WGS84', location = "bottomright")
#
#i_expert <- h_expert + ggsn::north(f.mun_50km, symbol=1,scale = 0.2,location = "bottomleft") +
#  theme(legend.text=element_text(size=7),
#        legend.title=element_text(size=8))

i_expert<- f_expert + geom_polygon (data=pampa,aes(x=long, y=lat, group=group), 
                                    colour = "black", 
                                    fill=NA,
                                    size=1,
                                    linetype = "dashed")

pdf(here ("output","FigS1-1.pdf"),width=7,height=7)
i_expert
dev.off()

# ----------------------------------------------------------------- #
## avestruz


load(here("data","organized_data", "dados_covariaveis.RData"))


## osbrepor o expert com mapa do RS
cores_avestruz <- data.frame (cores= presenca_avestruz$Avestruzes,
                            NM_MUNICIP=shape_RS$NM_MUNICIP)
sum(cores_avestruz$cores)

# fortify 
f.mun_avestruz <- cbind (f.mun, 
                       Nespecies= cores_avestruz [match (f.mun$id, 
                                                       cores_avestruz$NM_MUNICIP),]$cores)

## inserir deteccoes do GBIF
c_avestruz <-   b + geom_polygon(data=f.mun_avestruz, aes(x=long, y=lat, group=group, 
                                                      color=Nespecies, fill=Nespecies), 
                               colour = NA, size=1) + 
  labs (title= "Avestruz") +
  scale_fill_gradient2 (low='white', high='darkred', #midpoint= 0.20,
                        na.value = "gray85",
                        limits=c(0,1), 
                        breaks=seq(0,max(cores_avestruz$cores,na.rm=T),by=1),
                        name="Detected") ## para continuo

f_avestruz <- c_avestruz +
  xlab("") + ylab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan", 
                                        colour = "lightcyan", 
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(size=9),
        #legend.title = element_blank(),
        #legend.text = element_blank(),
        #legend.position = "none",
        axis.text=element_text(size=3),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 4),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 4),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-4,0, -0.5, 0), "lines"))
# top, right, bottom, and left margins

pdf(here ("output","avestruz.pdf"),width=7,height=7)
f_avestruz
dev.off()

# garbage


## extrair legenda
require(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legenda_comum <- get_legend(f_50km_legend) ## tem que primeiro gerar um mapa com legenda

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



