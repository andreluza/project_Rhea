

# load packages
source("R/packages.R")

# the first steps are locked because they took time
# avesRS_vertnet <- searchbyterm(class = "Aves",limit=9000,
# 	stateprovince=c("Rio Grande"))#,country = "Brazil",	year = ">=2000")
# 
# # vertmap(avesRS_vertnet,region=c("Brazil","Argentina","Paraguay","Uruguay","Bolivia"))
# # lat long data
# 
# aves_vertnet_data <- avesRS_vertnet$data
# 
# ## filter by state
# 
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio de Janeiro",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio De Janeiro",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande de Norte",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande do Norte",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande Do Norte",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande Do Norte State",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Negro",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "R?o Negro",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "R?o Grande",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande",]
# aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$stateprovince != "Rio Grande Do Sul ?",]
# 
# save.image("dados_vertnet.RData")

#load(here ("data","deteccoes","vertnet","dados_vertnet.RData"))

## data cleaning
## remove spp at class level
# checking
#cbind(aves_vertnet_data$genus,
#	aves_vertnet_data$specificepithet,
#	aves_vertnet_data$scientificname)

#aves_vertnet_data<-aves_vertnet_data [aves_vertnet_data$scientificname != "Aves",]
## remove spp lacking epithet (genera, not idetified  "sp.")
#aves_vertnet_data<-aves_vertnet_data [is.na(aves_vertnet_data$specificepithet) != T,]
## remove missing coords
#aves_vertnet_data<-aves_vertnet_data [is.na(aves_vertnet_data$decimallongitude) != T,]
#aves_vertnet_data$decimallongitude<-as.numeric(aves_vertnet_data$decimallongitude)
#aves_vertnet_data$decimallatitude<-as.numeric(aves_vertnet_data$decimallatitude)

## coordinate cleaner
# flag data to remove
#flags_spatial <- CoordinateCleaner::clean_coordinates(
#  x = aves_vertnet_data, 
#  species = "scientificname",
#  lon = "decimallongitude", 
#  lat = "decimallatitude",
#  tests = c("capitals", # radius around capitals
#            "centroids", # radius around country and province centroids
#            "duplicates", # records from one species with identical coordinates
#            "equal", # equal coordinates
#		"equador", ## points at  lat 0
#            "institutions", # radius around biodiversity institutions
#            "seas", # in the sea
#            "urban", # within urban area
#            "validity", # outside reference coordinate system
#            "zeros" # plain zeros and lat = lon
            
#  )
#)

# results
# flags_spatial %>% head
# summary(flags_spatial)
# flags_spatial$.summary

# exclude records flagged by any test
# aves_vertnet_data <- aves_vertnet_data %>% 
#  dplyr::filter(flags_spatial$.summary)
# aves_vertnet_data
#save (aves_vertnet_data,file="aves_vertnet_filtrado.RData")


# load cleaned data
load(here ("data", "detections", "vertnet","aves_vertnet_filtrado.RData"))

## spp list
sp_RS_vertnet <- unique (aves_vertnet_data$scientificname)
dados_vertnet <- lapply (sp_RS_vertnet, function (i)
  data.frame (aves_vertnet_data [which(aves_vertnet_data$scientificname == i),])
)

# overlap with  RS municipalities
require(rgdal)
require(raster)
shape_RS <- readOGR(dsn=here("data","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)

## remove lakes
shape_RS <- shape_RS [-c(96,250),]

## shape_RS
data.frame(dados_vertnet[[1]]$decimallatitude)
data.frame(dados_vertnet[[1]]$decimallongitude)

### remove coords beyond RS limits
vertnet_aves_RS <- lapply (dados_vertnet, function (i) {
  
  dados_aves_pais <- i [which(i$decimallatitude > extent(shape_RS)[3] & i$decimallatitude < extent(shape_RS)[4]),]
  dados_aves_pais <- dados_aves_pais[which(dados_aves_pais$decimallongitude > extent(shape_RS)[1] & dados_aves_pais$decimallongitude < extent(shape_RS)[2]),]
}
)
# filter
vertnet_aves_RS <- vertnet_aves_RS [which(unlist(lapply (vertnet_aves_RS,nrow)) >0)]

## coords of each record
coordenadas <- lapply (vertnet_aves_RS, function (i)  ## para cada sp de ave,
  i [,c("decimallongitude", "decimallatitude")])

coordenadas <- lapply (coordenadas, function (i) {
  coordinates (i) <- ~ decimallongitude + decimallatitude
  crs(i)<- crs(shape_RS)
  ; i
}
)

#plot(shape_RS,border="gray60",main="Vertnet")
#lapply(coordenadas, function (i) points(i,pch=19,cex=1,col="black"))
### pontos da ema (um deles eh Rhea americana intermedia)
#lapply(coordenadas [grep("Rhea",unlist(lapply (vertnet_aves_RS, function(i) i$scientificname[1])))], function(i)
#  points(i,col="red",pch=19))


## rhea detections
ema_vertnet <- vertnet_aves_RS [grep("Rhea",unlist(lapply (vertnet_aves_RS, function(i) i$scientificname[1])))]

### spatialpoints df to overlap with RS map
coordenadas_ema <- lapply (ema_vertnet, function (i) 
  i [,c("decimallongitude", "decimallatitude")]
)
coordenadas_ema<- do.call (rbind,coordenadas_ema)
## 
coordinates (coordenadas_ema) <- ~ decimallongitude + decimallatitude
crs(coordenadas_ema)<- crs(shape_RS)

# rhea detections
mun_ema <- over (coordenadas_ema,shape_RS)

# the map of municipalities with rhea detections
shape_mun_ema <- shape_RS [which (shape_RS@data$NM_MUNICIP %in% mun_ema$NM_MUNICIP),]

## dataframe
det_ema <- data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% shape_mun_ema@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP)

## check site correspondence
## det_ema$mun[which(det_ema$det == 1)] == shape_mun_ema@data$NM_MUNICIP

## remove records beyond RS limits
over_vertnet <- lapply (coordenadas, function (i)
  
  over (i,shape_RS)
  
) 

# each species records
mun_det_especies <- lapply (coordenadas,function (especie)
  over (especie,shape_RS))

# map of municipalities with detection of each spp
shape_mun_especies <- lapply (mun_det_especies, function (especie)
  shape_RS [which (shape_RS@data$NM_MUNICIP %in% especie$NM_MUNICIP),])

# analysis dataframe
det_especies <- lapply (shape_mun_especies, function (especie)
  data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
              det = ifelse (shape_RS@data$CD_GEOCMU %in% especie@data$CD_GEOCMU,1,0),
              mun = shape_RS@data$NM_MUNICIP))

## naming the lists of spp
names(det_especies) <- unlist(lapply (vertnet_aves_RS, function(i) i$scientificname[1]))

## melt the list 
## to count the number of spp
df_todas_sp <- do.call (cbind, lapply (det_especies, function (especie) 
  especie$det))
rownames(df_todas_sp) <- det_especies[[1]]$mun # naming the list (all lists with the same structure)

## obtain species richness
riqueza_aves_mun_RS <- rowSums (df_todas_sp)

## check site order
shape_RS$NM_MUNICIP == names(riqueza_aves_mun_RS)

# bind data
dados_det_ema_vertnet <- cbind (det_ema, riqueza_aves= riqueza_aves_mun_RS)

# plot
plot(shape_RS,border="black",main="Vertnet",
     col=rgb(dados_det_ema_vertnet$det,0,1,alpha=0.5))

# save
save (dados_det_ema_vertnet, file= here ("data","organized_data","input_VERTNET.RData"))

rm(list=ls())



