# extract the values from a raster at position of points
rm(list = ls(all = TRUE))
library(raster)
library(sf)

# input points
p_in = "Daten/Paldau/Landslides/Paldau_source_point.shp"
path_to_points = normalizePath(path = p_in, winslash = "\\", mustWork = NA)
points = st_read(path_to_points)

plot(points)

# input folder of filtered images
p_in = "Daten/Paldau/Parameters/filtered_images_slope"
path_to_images =  normalizePath(path = p_in, winslash = "\\", mustWork = NA)

# output folder
name = "filtered_slope_pointVal"
p_out = "Daten/Paldau/Parameters/"
path_to_out =  normalizePath(path = p_out, winslash = "\\", mustWork = NA)

# list of all images in folder
fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE)

# create raster stack of all filterd images
rasStack = stack(fs)
# extract the values from the raster. This may take a while
rasValue=extract(rasStack, points)
rasValue

# write to csv
combinePointValue=cbind(points,rasValue)
write.table(combinePointValue, file = paste0(path_to_out, name,".csv"), append=FALSE,
            sep= ",", row.names = FALSE, col.names=TRUE)

# save as R object
saveRDS(combinePointValue, paste0(path_to_out,name,".rds"))
plot(combinePointValue)
