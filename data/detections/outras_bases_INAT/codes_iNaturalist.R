

# -----------------------------------
#         iNaturalist



# load packages
source("R/packages.R")



############### other datasets
# unlock these lines to download the data
## bases for download
#db <- c(#"gbif",       # Global Biodiversity Information Facility (https://www.gbif.org/)
#        #"ecoengine",  # Berkeley Initiative for Global Change Biology (https://ecoengine.berkeley.edu/)
#        "inat",       # iNaturalist (https://www.inaturalist.org/)
#        "vertnet",    # VertNet (http://vertnet.org/)
#        "ebird",      # eBird (https://ebird.org/)
#        "idigbio"    # Integrated Digitized Biocollections (https://www.idigbio.org/)
#        #"obis",       # Ocean Biogeographic Information System (www.iobis.org)
#        #"ala",        # Atlas of Living Australia (https://www.ala.org.au/)
#        #"bison"       # Biodiversity Information Serving Our Nation (https://bison.usgs.gov)
#)
#
#db
#
## occ download
#require (spocc)
#
#bounds<- c(extent (shape_RS) [1],extent (shape_RS) [2],extent (shape_RS) [3],extent (shape_RS) [4])
#
### para cada sp da lista de SP do GBIF, extrair as informacoes do INAT
#
#occ <- lapply (as.character(especies_aves_RS), function (i)
#                  occ(query = i, 
#                  from = db, 
#                  ebirdopts = list(key = "6dvjcvmgf2es"), # make key in https://ebird.org/api/keygen
#                  has_coords = TRUE, 
#                  limit = 1e5, geometry=bounds))                  
## bounding box following this : https://arthur-e.github.io/Wicket/sandbox-gmaps3.html 

#save(occ, file="avesRS_varias_DB.RData")

load (here ("data","detections","outras_bases_INAT","avesRS_varias_DB.RData"))

#'Rhea americana'

occ_to_df <- lapply (occ, function (i)
    occ2df(i,what="all")) #https://www.rdocumentation.org/packages/spocc/versions/1.0.2/topics/occ2df

## INAT data
## unique (unlist(lapply (occ_to_df, function (i) unique (i$data$prov))))

## transform coordinates (character to numeric)
occ_to_df<- lapply (occ_to_df, function (i) { ## for each species
    i$data$longitude <- as.numeric (i$data$longitude)  # edit
    i$data$latitude <- as.numeric (i$data$latitude)# edit
    
    ;i
    }
)

## remove species lacking records
occ_to_df <- occ_to_df [unlist(lapply (occ_to_df,function (i) dim (i$data) [1] == 0))==F]

# spatial analysis
require(rgdal)
require(raster)
shape_RS <- readOGR(dsn=here("data","shape_munRS"), layer="43MUE250GC_SIR",
                    encoding = "UTF-8",use_iconv = T)
## remove lakes
shape_RS <- shape_RS [-c(96,250),]

### remove coords beyond RS limits
INAT_aves_RS <- lapply (occ_to_df, function (i) {

    dados_aves_pais <- i$data [which(i$data$latitude > extent(shape_RS)[3] & i$data$latitude < extent(shape_RS)[4]),]
    dados_aves_pais <- dados_aves_pais[which(dados_aves_pais$longitude > extent(shape_RS)[1] & dados_aves_pais$longitude < extent(shape_RS)[2]),]
}
)

## remove too recent data (collected after 31-12-2018)
INAT_aves_RS <- lapply (INAT_aves_RS, function (i) 
  i [ which(i$date <= as.Date ("2018-12-31")),]
)

## remove old data (collected before 01-01-2008)
INAT_aves_RS <- lapply (INAT_aves_RS, function (i) 
  i [ which(i$date >= as.Date ("2008-01-01")),]
)

## remove species lacking records
INAT_aves_RS <- INAT_aves_RS [unlist (lapply(INAT_aves_RS, nrow))>0]

## coordinates of each record
coordenadas <- lapply (INAT_aves_RS, function (i)  ## each species i
    i [,c("longitude", "latitude")])

coordenadas <- lapply (coordenadas, function (i) {
    coordinates (i) <- ~ longitude + latitude
    crs(i)<- crs(shape_RS)
    ; i
}
)

### remove coords beyond RS limits
over_INAT <- lapply (coordenadas, function (i)
    
    over (i,shape_RS)
)

over_INAT <- lapply (seq (1,length (over_INAT)) , function (i)
    
    INAT_aves_RS [[i]] [which(is.na(over_INAT [[i]]$NM_MUNICIP)==F),]
)

### filter once again
INAT_aves_RS <- over_INAT [unlist (lapply(over_INAT, nrow))>0]

## Rhea americana data
dados_rhea <- INAT_aves_RS [[grep ("Rhea",unlist(lapply (INAT_aves_RS, function (i) i[,1][1])))]]
# to spatial data
spdf <- dados_rhea[,2:3]
coordinates(spdf) <- ~longitude+latitude
crs(spdf) <- crs(shape_RS)
# plot
##
plot(shape_RS,border="black",main="INat",
     col = rgb(ifelse (is.na(over (shape_RS,spdf)),0,1),0,1,alpha=0.5))
#lapply (INAT_aves_RS, function (i)
#  points(i$longitude,i$latitude,pch=19)
#)
#
#
#points(dados_rhea$longitude,dados_rhea$latitude,pch=19,col="red")
#

### producing spatialpoints dataframe with Rhea data  to overlap on the RS map
coordenadas_ema <- dados_rhea [,c("longitude", "latitude")]
coordinates (coordenadas_ema) <- ~ longitude + latitude
crs(coordenadas_ema)<- crs(shape_RS)

# municipalities with detection
mun_ema <- over (coordenadas_ema,shape_RS)
# get the map of municipalities with detection
shape_mun_ema <- shape_RS [which (shape_RS@data$NM_MUNICIP %in% mun_ema$NM_MUNICIP),]

## detection dataframe
det_ema <- data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                       det = ifelse (shape_RS@data$CD_GEOCMU %in% shape_mun_ema@data$CD_GEOCMU,1,0),
                       mun = shape_RS@data$NM_MUNICIP)

## check correspondence
## det_ema$mun[which(det_ema$det == 1)] == shape_mun_ema@data$NM_MUNICIP

# richness of other species
## coords of each sp
coordenadas <- lapply (INAT_aves_RS, function (i)  ## for each species
    i [,c("longitude", "latitude")]) # extract coords

coordenadas <- lapply (coordenadas, function (i) {
    coordinates (i) <- ~ longitude + latitude
    crs(i)<- crs(shape_RS)
    ; i
}
)

# overlap
mun_det_especies <- lapply (coordenadas,function (especie)
    over (especie,shape_RS))

# a map for each sp
shape_mun_especies <- lapply (mun_det_especies, function (especie)
    shape_RS [which (shape_RS@data$NM_MUNICIP %in% especie$NM_MUNICIP),])

# detection / non-detection dataframe
det_especies <- lapply (shape_mun_especies, function (especie)
    data.frame (cod_mun = shape_RS@data$CD_GEOCMU,
                det = ifelse (shape_RS@data$CD_GEOCMU %in% especie@data$CD_GEOCMU,1,0),
                mun = shape_RS@data$NM_MUNICIP))

## naming the list
names(det_especies) <- unlist(lapply (INAT_aves_RS, function (i) i[,1][1]))

## melt the list
df_todas_sp <- do.call (cbind, lapply (det_especies, function (especie) 
    especie$det))

## to count the number of species in each municipality
rownames(df_todas_sp) <- det_especies[[1]]$mun # all list elements have the same structure
## count
riqueza_aves_mun_RS <- rowSums (df_todas_sp)

## check the order
# shape_RS$NM_MUNICIP == names(riqueza_aves_mun_RS)


#save
dados_det_ema_inat <- cbind (det_ema, riqueza_aves= riqueza_aves_mun_RS)
save (dados_det_ema_inat, file= here ("data","organized_data","input_INAT.RData"))


rm(list=ls())
