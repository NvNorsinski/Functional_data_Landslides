# sample non landslides from raster
rm(list = ls(all = TRUE))
library(raster)
library(sf)
library(sp)
library(rgeos)
#library(dismo)

# input path
path_to_images = "Daten/Paldau/Parameters/Filtered_images_twi"
path_to_giant = "Daten/Paldau/Landslides/Giant_Paldau.shp"
path_to_minor = "Daten/Paldau/Landslides/Landslides_Paldau_02_18.shp"
path_rnd_poi = "Daten/Paldau/Samples/randomPoints.rds"

# ouput path
path_to_out = "Daten/Paldau/Samples"
name_mask = "maskPaldau"
name_random_values = "rnd_no_lsd_Paldau_twi"

# read images
giant = st_read(path_to_giant)
minor = st_read(path_to_minor)

# union of different polygons
giant = as(giant, 'Spatial')
minor = as(minor, "Spatial")
lanslde_poly = gUnion(giant, minor)

# create raster stack
fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE)
fs
rasStack = stack(fs)

# only run this section if no mask layer is calculatet  ------------------------
# create mask image

dim = dim(rasStack)
r = raster(ncol= dim[2], nrow=dim[1])
extent(r) = extent(rasStack)
r
mask = rasterize(lanslde_poly, r)

# change value where is no polygon to 2
mask[is.na(mask[])] = 2
# change values where a polygon is to NA
mask[mask == 1] = NA
plot(mask)
# change twos back to ones. In fact this is not necessary
mask[mask == 2] = 1


crs(mask) = crs(rasStack)

# write mask to disk
writeRaster(mask, filename=file.path(path_to_out, name_mask),
            format = "GTiff", overwrite = TRUE)

# same random points for all samplings
#rnd_poi = as.data.frame(randomPoints(mask = rasStack, n = 400))
#rnd_poi

# saveRDS(rnd_poi, "Daten/Paldau/Samples/randomPoints.rds")

# end of region not to run------------------------------------------------------
rnd_poi = readRDS(path_rnd_poi)

# create new raster Stack, with mask image as first image
fs = c(paste0(path_to_out,"/", name_mask,".tif"), fs)
fs

rasStack = stack(fs)

# two way of sampling random points
# first one does not need an additional package beside raster

#start_time = Sys.time()

#samp = sampleRandom(rasStack, size = 100, na.rm = TRUE)
#samp
#end_time = Sys.time()

#time = end_time - start_time
#time

# second method is based on dismo package
# it is faster and takes in to account that the cell size is different,
# depending on distance to equator. So it is preferable

#start_time = Sys.time()



ext = extract(rasStack, rnd_poi)
ext

#end_time = Sys.time()

#time = end_time - start_time
#time

samples_df = as.data.frame(t(ext))

saveRDS(samples_df, paste0(path_to_out,"/", name_random_values,".rds"))
