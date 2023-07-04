
#setwd("C:/Miller/Projects/Brazil Viviane")
#setwd("C:/Users/vivia/Dropbox/Viviane Parrots/Data")

setwd("G:/Meu Drive/Data Integration Models/data2019/Data Models/Data2020")

library(spdep)
library(maps)
library(maptools)
library(rgdal)
library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
#set.seed(1)


## Now create adjacency matrices from shapefiles
## Read shapefile and create polygonal list of the map
#muni = readShapeSpatial("/Data/NewDistrArea.shp")
muni = readOGR("DISTRIBUTION.shp")
#muni = readOGR("testeClipDistribution3.shp")
muni.centroids <- getSpPPolygonsLabptSlots(muni)

make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}


hex <- make_grid(muni, cell_diameter = .5)
#hex <- make_grid(muni, cell_diameter = 1)
#hex <- make_grid(muni, cell_diameter = .25)
hex.centroids <- getSpPPolygonsLabptSlots(hex)


cell.id <- c()
for (a in 1:nrow(muni.centroids)){
 cell.id[a] <- which.min(sqrt((hex.centroids[,1]-muni.centroids[a,1])^2+(hex.centroids[,2]-muni.centroids[a,2])^2))
}



## Convert the polygonal representation into a neighborhood list 
hex.nb = poly2nb(hex)
num <- sapply(hex.nb,length)
sumNeigh <- sum(num)
adj = unlist(hex.nb)

save.image("CARparams.RData")
