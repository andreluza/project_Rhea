## -------------
# dados dos especialistas
require(here)
require(raster)
require(rgdal)

RS_shape <- readOGR(dsn=here ("data", "shape_munRS"), layer="43MUE250GC_SIR")
files_expert <- list.files(here ("data", "expert_opinion"),pattern=".rds")

## expert data
expert_data <- lapply (files_expert, function (i)
  readRDS (here ("data", "expert_opinion", i)))

# plottong
#plot(RS_shape,border="gray", main="Expert knowledge",)
#lapply (expert_data, function (i)
#  
#    plot(i$Shape_Mun,add=T,col=rgb(1,0,1,alpha=0.5))
#)
#
#legend ('bottomleft',legend =c("Presence of Rhea",
#                               "Absence of Rhea"),
#        col = c("black","white"),pch=15,bty="n")

crs(RS_shape) <- crs(expert_data[[1]]$Shape_Mun)
#plot(expert_data[[1]]$Shape_campo,add=T, col="green")
# rhea (expert presence)
# over each expert opinion on RS shape
expert_rhea<-lapply (expert_data, function (i) 
  over (RS_shape,i$Shape_Mun))
# melt the list
expert_rhea<- do.call(cbind,expert_rhea)
# adjusting
expert_rhea<- ifelse(is.na(expert_rhea),0,1)
# plot

plot(RS_shape,border="black", main="Expert knowledge",
     col = rgb (rowSums(expert_rhea)>1,0,1,alpha=0.5))
legend ('bottomleft',legend =c("Presence of Rhea",
                               "Absence of Rhea"),
        col = c("purple","blue"),pch=15,bty="n")


