
## greater rhea distribution modeling
## GBIF data

# load packages
source("R/packages.R")


############################################################################
###############       DATA GBIF        ####################################
############################################################################

# OBS : unzip the file "0006456-200221144449610" to the folder and load the data unlocking functions below
# the file is around 300 MB size

#dados <- read.csv (here ("deteccoes", "Gbif","0006456-200221144449610.csv"),sep="\t",#fill = TRUE,
#                  strip.white=T,quote ="\t",h=T)# 
# need  "Animalia	Chordata Aves"
#dados_aves <- dados [which(dados$class == "Aves"),]
#write.csv(dados_aves,file="GBIF_aves_RS.csv") # save heavy data
#dados_aves<- read.csv(here("deteccoes", "Gbif","GBIF_aves_RS.csv")) # load again to cleaning
 
# ########################################
# ######## DATA CLEANING    ##############
# ########################################
#dados_aves <- dados_aves [which (is.na(dados_aves$decimalLatitude) == F),]
#dados_aves <- dados_aves [which (is.na(dados_aves$decimalLongitude) == F),]
 
# cleaning
#flag data to remove
#flags_spatial <- CoordinateCleaner::clean_coordinates(
#     x = dados_aves, 
#     species = "species",
#     lon = "decimalLongitude", 
#     lat = "decimalLatitude",
#     tests = c("capitals", # radius around capitals
 #              "centroids", # radius around country and province centroids
 #              "duplicates", # records from one species with identical coordinates
 #              "equal", # equal coordinates
 #              "equador", ## points at  lat 0
 #              "institutions", # radius around biodiversity institutions
 #              "seas", # in the sea
 #              "urban", # within urban area
 #              "validity", # outside reference coordinate system
 #              "zeros" # plain zeros and lat = lon
 #              
 #    ) 
 #)
 
# filter results
#require(dplyr)
#flags_spatial %>% head
#summary(flags_spatial)
#flags_spatial$.summary
 
# exclude records flagged by any test
#dados_aves_filtrado <- dados_aves %>% 
# 
#        dplyr::filter(flags_spatial$.summary)

## identificar coordenadas fora do BR
#dados_aves_pais <- dados_aves_filtrado [-which (dados_aves_filtrado$countryCode == "AR"),]
#dados_aves_pais$countryCode <- droplevels(dados_aves_pais$countryCode)
# save 
#write.csv(dados_aves_filtrado,file="GBIF_aves_RS_filtrado.csv")
#save(dados_aves_filtrado, file="GBIF_aves_RS_filtrado.RData")

# ------------------------------------------------------------------------------------
# load cleaned data to further formating
 
load(here ("data","detections","Gbif","GBIF_aves_RS_filtrado.RData"))

## remove old data  ## range EBIRD "2008-01-25" "2018-12-31"
dados_aves_filtrado <- dados_aves_filtrado [which (as.Date (as.character(dados_aves_filtrado$eventDate)) >= as.Date ("2008-01-01")),]
## remove recent data  - similar to ebird 
dados_aves_filtrado <- dados_aves_filtrado [which (as.Date (as.character(dados_aves_filtrado$eventDate)) <= as.Date ("2018-12-31")),]

## read study area map
require(rgdal)
require(raster)
shape_RS <- readOGR(dsn=here("data","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## remove lakes
shape_RS <- shape_RS [-c(96,250),]

### remove coordinates beyond RS limits
dados_aves_pais <- dados_aves_filtrado[which(dados_aves_filtrado$decimalLatitude > extent(shape_RS)[3] & dados_aves_filtrado$decimalLatitude < extent(shape_RS)[4]),]
dados_aves_pais <- dados_aves_pais[which(dados_aves_pais$decimalLongitude > extent(shape_RS)[1] & dados_aves_pais$decimalLongitude < extent(shape_RS)[2]),]

### coordinates of detection
coordenadas <- dados_aves_pais [,c("decimalLongitude", "decimalLatitude")]
coordinates (coordenadas) <- ~ decimalLongitude + decimalLatitude
crs(coordenadas)<- crs(shape_RS)


## overlap coordinates on RS map
over_coord_RS <- over (coordenadas,shape_RS)
dados_aves_pais <-dados_aves_pais [which(is.na(over_coord_RS$NM_MUNICIP)==F),]
 
# load packages for spatial analysis 
library(sp)
library(raster)
library(maps)
library(rgdal)
library(maptools)
library(rgbif)
library(rvertnet)

## Rhea americana detection
dados_rhea <- dados_aves_pais [which(dados_aves_pais$species == "Rhea americana"),]

### spatialpoints dataframe with Rhea data
coordenadas_ema <- dados_rhea [,c("decimalLongitude", "decimalLatitude")]
coordinates (coordenadas_ema) <- ~ decimalLongitude + decimalLatitude
crs(coordenadas_ema)<- crs(shape_RS)

# municipalities with GRhea detection
mun_ema <- over (coordenadas_ema,shape_RS)

# filter municipalities with rhea
shape_mun_ema <- shape_RS [which (shape_RS@data$NM_MUNICIP %in% mun_ema$NM_MUNICIP),]

#plot(shape_RS,border="gray60",main="Dados GBIF")
plot(shape_RS,
     col = rgb(red = ifelse(shape_RS$NM_MUNICIP %in% mun_ema$NM_MUNICIP,1,0), 
                             green = 0, blue = 1, alpha = 0.5),
     border = "black",
     lwd=1,
     main="GBIF Data")

legend ('bottomleft',legend =c("Detection of Rhea",
                               "No detection of Rhea"),
        col = c("purple","blue"),pch=15,bty="n")

# plot of all sampling points, and points with rhea

#points(dados_aves_pais$decimalLongitude,
#       dados_aves_pais$decimalLatitude,
#       pch=1,cex=0.7,col="gray40")
#
# pontos com RHea
#points(dados_rhea$decimalLongitude,dados_rhea$decimalLatitude,
#       col="red",pch=1,cex=0.7)## so pra dar uma engrossada na espessura da linha
#points(dados_rhea$decimalLongitude,dados_rhea$decimalLatitude,
#       col="red",pch=1,cex=0.7)

### detection dataframe, municipality in the row, detection \ non-detection in the col
det_ema <- data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% shape_mun_ema@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP)

## check id matching 
# det_ema$mun[which(det_ema$det == 1)] == shape_mun_ema@data$NM_MUNICIP

## now we need a dataframe with total species richness per municipality (sampling effort)
## 1st - spatialdataframe with coords of each species in the dataset

## list of species
especies_aves_RS <- unique (dados_aves_pais$species)

## filter data for each species
subconjunto_aves_RS <- lapply (especies_aves_RS, function (especie)
        dados_aves_pais [which (dados_aves_pais$species == especie),])

## coordinates of each sp
coordenadas_especies <- lapply (subconjunto_aves_RS, function (especie)
        especie [,c("decimalLongitude", "decimalLatitude")])

## transform all objects into spatialpoints 
coordenadas_especies <- lapply (coordenadas_especies, function (especie) {
    coordinates (especie) <- ~ decimalLongitude + decimalLatitude
    crs(especie)<- crs(shape_RS); ## reference proj
    especie # spdf of each sp.

 }
)

# overlap the coordinates of each species with RS map
mun_det_especies <- lapply (coordenadas_especies,function (especie)
                            over (especie,shape_RS))

# the RS map for each species
shape_mun_especies <- lapply (mun_det_especies, function (especie)
        shape_RS [which (shape_RS@data$NM_MUNICIP %in% especie$NM_MUNICIP),])

#  finaly we have the dataframe with municipalities in the rows and each species detection/non-detection in the cols
det_especies <- lapply (shape_mun_especies, function (especie)
        data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% especie@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP))
## naming the list
names(det_especies) <- especies_aves_RS

## melt the list to have the N spp per municipality
df_todas_sp <- do.call (cbind, lapply (det_especies, function (especie) 
        especie$det))

#naming
rownames(df_todas_sp) <- det_especies[[1]]$mun

## species richness per municipality
riqueza_aves_mun_RS <- rowSums (df_todas_sp)


########################
## check correspondence of sites in Species richness dataset and rhea dataset
#names (riqueza_aves_mun_RS) ==  det_ema$cod_mun

# save
dados_det_ema_gbif <- cbind (det_ema, riqueza_aves= riqueza_aves_mun_RS)
save (dados_det_ema_gbif, file= here("data","organized_data","input_GBIF.RData"))
rm(list=ls())
