## -------------
# dados dos especialistas
require(here)
require(raster)
require(rgdal)


RS_shape <- readOGR(dsn=here ("data", "shape_munRS"), layer="43MUE250GC_SIR")

data1 <- readRDS (here ("data", "expert_opinion", "Dias_RA_2020-12-18.rds"))

plot(RS_shape,border="gray")
plot(data1$Shape_Mun,add=T,col="red")
plot(data1$Shape_campo,add=T, col="green")

