# extract the values from a shape file at position of points
rm(list = ls(all = TRUE))
library(sf)

# input points

path_to_points = "Daten/Paldau/Landslides/Paldau_source_point.shp"
points = st_read(path_to_points)

# input folder of shape
path_to_shape = "Daten/Paldau/Geology/Geologie_GIS_Paldau.shp"
shape = st_read(path_to_shape)
#plot(shape)

# output folder
name = "geology_pointVal"
path_to_out = "Daten/Paldau/Samples/"


geolgy = st_join(points, shape, join = st_intersects)
geolgy

saveRDS(geolgy, paste0(path_to_out, name,".rds"))

