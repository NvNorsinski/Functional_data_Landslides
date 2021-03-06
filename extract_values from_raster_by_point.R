# Author: Nils von Norsinski
# extract the values from a raster at position of points
rm(list = ls(all = TRUE))
library(raster)
library(sf)

# input points

path_to_points = "Daten/Paldau/Landslides/Cliped_landslide_points.shp"
points = st_read(path_to_points)

# input folder of filtered images
path_to_images = "Daten/Paldau/Parameters/filtered_images_aspect_ow"

# output folder
name = "filtered_aspectow_pointVal"
path_to_out = "Daten/Paldau/Samples/"

# list of all images in folder
fs = list.files(path = path_to_images, pattern = "tif$", full.names = TRUE)
fs
# create raster stack of all filterd images
rasStack = stack(fs)
# extract the values from the raster. This may take a while
rasValue = extract(rasStack, points)
rasValue

combinePointValue = cbind(points, rasValue)


# save as R object
saveRDS(combinePointValue, paste0(path_to_out, name,".rds"))

