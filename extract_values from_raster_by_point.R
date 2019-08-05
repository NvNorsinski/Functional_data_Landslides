# extract the values from a raster at position of points
rm(list = ls(all = TRUE))
library(raster)
library(sf)

# input points

path_to_points = "Daten/Paldau/Landslides/Paldau_source_point.shp"
points = sf::st_read(path_to_points)

# input folder of filtered images
path_to_images = "Daten/Paldau/Parameters/filtered_images_slope"

# output folder
name = "filtered_slope_pointVal"
path_to_out = "Daten/Paldau/Parameters/"

# list of all images in folder
fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE)

# create raster stack of all filterd images
rasStack = raster::stack(fs)
# extract the values from the raster. This may take a while
rasValue = extract(rasStack, points)
rasValue

# write to csv
 combinePointValue=cbind(points,rasValue)
 write.table(combinePointValue, file = paste0(path_to_out, name,".csv"),
             append = FALSE, sep= ",", row.names = FALSE, col.names = TRUE)

# save as R object
saveRDS(combinePointValue, paste0(path_to_out, name,".rds"))

