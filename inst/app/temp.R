
library(raster)
shp_data = shapefile('data/shp/MunicipiosColombia_geo.shp')
plot(shp_data, col='dodgerblue',main='a) shp_data',lty=0)

shp_data@data

l1 <- shp_data@polygons[[1]]@Polygons[[1]]@coords
l1_coord <- data.frame(lat = l1[,1], long = l1[,2])
#ifelse()