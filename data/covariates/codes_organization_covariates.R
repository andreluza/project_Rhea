#### ----------------------------------
## site occupancy covariates at municipality scale


# load packages
source("R/packages.R")



# study area RS map
shape_RS <- readOGR(dsn=here("data","covariates","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv=T)

## remove the lakes
#plot(shape_RS [-c(96,250),],col="red")
shape_RS <- shape_RS [-c(96,250),]
# shape_RS$NM_MUNICIP <- as.character (shape_RS$NM_MUNICIP)
#shape_RS$NM_MUNICIP [which (shape_RS$NM_MUNICIP == "SANT'ANA DO LIVRAMENTO")] <- "SANTANA DO LIVRAMENTO"

# read
uso_cob_solo <- readOGR(dsn=here("data","covariates","vegetacao_rs_2015_1km_v1_lambert"), layer="vegetacao_rs_2015_1km_v1_lambert",
                        encoding = "UTF-8",use_iconv=T)

# reproject
shape_RS <- spTransform(shape_RS, crs(uso_cob_solo), check=T)

## buffer tp extract
#plot(uso_cob_solo [which(uso_cob_solo$LEGENDA == usos[16]),])
uso_cob_solo <- gBuffer(uso_cob_solo, byid=TRUE, width=0)
sum(gIsValid(uso_cob_solo, byid=TRUE)==FALSE)

## list to select forest and grassland
usos <- unique(uso_cob_solo$LEGENDA)

# parallel processing
cl <- makeCluster(7) ## number of cores = generally ncores -1
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(rgdal))

# export your data and function
clusterExport(cl, c("usos", "shape_RS","uso_cob_solo"))

usos_intersect <- parLapply(cl,usos, function (i) 
  intersect(shape_RS,uso_cob_solo[which (uso_cob_solo$LEGENDA == i),]))

stopCluster(cl)

# set names
names(usos_intersect)<- usos 

# cover of a few classes
plot(usos_intersect[[9]],
     main="Cover of natural grassland")
plot(usos_intersect[[6]],
     main="Cover of natural forest")

### dataframe with land uses
usos_tabela <- lapply (usos_intersect, function (i){
    
    i$area <- area(i)
    tab <- aggregate(area~NM_MUNICIP, data=i, FUN=sum)
    tab <- rbind (tab, data.frame (NM_MUNICIP=shape_RS$NM_MUNICIP[which (shape_RS$NM_MUNICIP %in% tab$NM_MUNICIP == F)],
                area=0))
    tab <- tab [match(shape_RS$NM_MUNICIP,tab$NM_MUNICIP),]

})

# each municipality in the rows, land uses in the cols

usos_tabela <- do.call (cbind,usos_tabela)
## extract/select area
usos_tabela <- usos_tabela [,grep("area", colnames (usos_tabela))]
rownames (usos_tabela) <- shape_RS$NM_MUNICIP

## extract the area of each municipality
area_municipios <-  gArea(shape_RS,byid=T)

## municipality proportion per municipality
usos_tabela <- usos_tabela/area_municipios

########################################
####  IBGE  DATA ####################
#### AGRICULTURE CENSUSES  ################
########################################

## Number of establishments for milking cows
numero_estabelecimentos_leite <- read.csv (here("data","covariates", "numero_estabelecimentos_leite_utf.csv"),header = TRUE,
                                           stringsAsFactors = T, 
                                           encoding = "utf-8",sep=";")
numero_estabelecimentos_leite$X <-toupper (numero_estabelecimentos_leite$X)

## Number of establishments that raise domestic animals
numero_de_propriedades_domesticos <- read.csv (here("data","covariates","numero_de_propriedades_domesticos_utf.csv"),h=T,sep=";")
numero_de_propriedades_domesticos$X <- toupper (numero_de_propriedades_domesticos$X)

## number of establishmets with crop fields
numero_estabelecimentos_lavoura <- read.csv (here("data","covariates","numero_estabelecimentos_lavoura_utf.csv"),
                                             h=T,sep=";")
numero_estabelecimentos_lavoura$X <-toupper (numero_estabelecimentos_lavoura$X)

## number (count) of domestic animals
numero_de_cabecas_domesticos <- read.csv (here("data","covariates","numero_de_cabecas_domesticos_utf.csv"),h=T,sep=";",
                                          stringsAsFactors=F)
numero_de_cabecas_domesticos$X <- toupper (numero_de_cabecas_domesticos$X)

## ostrich data
presenca_avestruz <- numero_de_cabecas_domesticos [,c(1,9)]
presenca_avestruz$Avestruzes <- ifelse (presenca_avestruz$Avestruzes == "-", 0,1)
presenca_avestruz <- rbind (presenca_avestruz,
                         data.frame (X= shape_RS$NM_MUNICIP [which(shape_RS$NM_MUNICIP %in% presenca_avestruz$X == F)],
                                     Avestruzes= 0))
## number of sheep (rhea is often seen with sheep)
numero_ovelhas <- numero_de_propriedades_domesticos [,c(1,7)]
numero_ovelhas <- rbind (numero_ovelhas,
                            data.frame (X= shape_RS$NM_MUNICIP [which(shape_RS$NM_MUNICIP %in% numero_ovelhas$X == F)],
                                        Ovinos= 0))

## number of cattle
numero_de_cabecas_domesticos$Bovinos <- as.numeric(numero_de_cabecas_domesticos$Bovinos)
numero_bovinos <- numero_de_cabecas_domesticos [,c(1,3)]
numero_bovinos <- rbind (numero_bovinos,
                               data.frame (X= shape_RS$NM_MUNICIP [which(shape_RS$NM_MUNICIP %in% numero_bovinos$X == F)],
                                           Bovinos= 0))

# number of cows for milking
numero_cabecas_leite <- read.csv (here("data","covariates","numero_cabecas_leite_utf.csv"),h=T,sep=";",
                                  stringsAsFactors=F)
numero_cabecas_leite$X <-toupper (numero_cabecas_leite$X)
numero_cabecas_leite$n_cabecas <- as.numeric(numero_cabecas_leite$n_cabecas)
# bind data
numero_cabecas_leite <- rbind (numero_cabecas_leite,
                               data.frame (X= shape_RS$NM_MUNICIP [which(shape_RS$NM_MUNICIP %in% numero_cabecas_leite$X == F)],
                                           n_cabecas= 0))
                                           
## number of beef cattle heads
numero_bovinos_corte <- data.frame(X=numero_cabecas_leite$X,
                                     n_cabecas=numero_bovinos$Bovinos - numero_cabecas_leite$n_cabecas)

## ordering
presenca_avestruz <- presenca_avestruz [match(shape_RS$NM_MUNICIP,presenca_avestruz$X),]
numero_bovinos_corte <- numero_bovinos_corte [match(shape_RS$NM_MUNICIP,numero_bovinos_corte$X),]
numero_estabelecimentos_lavoura <- numero_estabelecimentos_lavoura [match(shape_RS$NM_MUNICIP,numero_estabelecimentos_lavoura$X),]
numero_ovelhas <- numero_ovelhas [match(shape_RS$NM_MUNICIP,numero_ovelhas$X),]

## salvar os dados das covariaveis

save(presenca_avestruz,
     numero_bovinos_corte,
     numero_estabelecimentos_lavoura,
     numero_ovelhas,
     usos_tabela, 
     file=here ("data","organized_data","data_covariates.RData"))

#####################################################
#### SPATIAL PARAMETERS (SPATIAL AUTOCORRELATION)  ##
#####################################################


## Now create adjacency matrices from shapefiles
## Read shapefile and create polygonal list of the map
# muni = readShapeSpatial("/Data/NewDistrArea.shp")
# muni = readOGR("DISTRIBUTION.shp")
# muni = readOGR("testeClipDistribution3.shp")
muni.centroids <- getSpPPolygonsLabptSlots(shape_RS)

# load make hexagonal grid function
source ("R/make_grid_function.R")

# sizes
sizes <- c(10000, 25000, 50000, 75000)# 10, 25, 50, and 75 km
# create hexagonal grids
hex <- lapply (sizes, function (i) 
  
              make_grid(shape_RS, cell_diameter = i) 

              )
   

# extract centroid
hex.centroids <- lapply (hex, getSpPPolygonsLabptSlots)

# attributing ID of each cell neighbor
# previous function
# cell.id <- c()
#for (a in 1:nrow(muni.centroids)){
#  cell.id[a] <- which.min(sqrt((hex.centroids[,1]-muni.centroids[a,1])^2+(hex.centroids[,2]-muni.centroids[a,2])^2))
#}
# actual function
cell.id <- lapply (hex.centroids, function (k)
  
              unlist (lapply (seq (1,nrow(muni.centroids)), function (i)
  
                  which.min(sqrt((k[,1]- muni.centroids[i,1])^2+(k[,2]-muni.centroids[i,2])^2))  
                  )
              )
            )


## Convert the polygonal representation into a neighborhood list 
hex.nb <- lapply (hex,poly2nb)
# number of neighbors per cell
num <- lapply (hex.nb, function (i) 
  sapply(i,length)
  )
# total number of neighbors
sumNeigh <- lapply (num,sum)
# find adjacent cells
adj <- lapply (hex.nb,unlist)

## plot of the neighborhood
par (mfrow=c(2,2),mar=c(1,1,1,1))       
# example map
plot(hex[[2]],main="25km")
plot(hex.nb[[2]],
     cbind(coordinates(hex[[2]]) [,1], 
           coordinates(hex[[2]]) [,2]),add=T)

plot(hex[[3]],main="50km")
plot(hex.nb[[3]],
     cbind(coordinates(hex[[3]]) [,1], 
           coordinates(hex[[3]]) [,2]),add=T)

plot(shape_RS,border="black")
plot(hex[[4]],add=T,border="gray36")
points (muni.centroids[,1],
        muni.centroids[,2],
        pch=19,col=rgb(1,0.5,0.5,alpha=0.4))

# save these data
save(cell.id,
     hex.centroids,
     num,
     sumNeigh,
     adj,
     file=  here ("data","organized_data","CARparams.RData"))



rm(list=ls())