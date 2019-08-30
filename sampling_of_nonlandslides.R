# sample non landslides from raster
rm(list = ls(all = TRUE))
library(raster)
library(sf)
library(sp)
library(rgeos)


files = c("Filtered_images_aspect", "Filtered_images_aspect_sincos",
          "Filtered_images_catchmant_area", "Filtered_images_dgm",
          "Filtered_images_generalCurvature", "Filtered_images_planCurvature",
          "Filtered_images_slope", "Filtered_images_tpi", "Filtered_images_twi")

out_names = c("rnd_no_lsd_Paldau_aspect", "rnd_no_lsd_Paldau_aspect_sincos",
              "rnd_no_lsd_Paldau_catchmant_area", "rnd_no_lsd_Paldau_dgm",
              "rnd_no_lsd_Paldau_generalCurvature",
              "rnd_no_lsd_Paldau_planCurvature", "rnd_no_lsd_Paldau_slope",
              "rnd_no_lsd_Paldau_tpi", "rnd_no_lsd_Paldau_twi")


for (i in 1:length(files)){
# input path
path_to_images = paste0("Daten/Paldau/Parameters/", files[i])
path_to_giant = "Daten/Paldau/Landslides/Giant_Paldau.shp"
path_to_minor = "Daten/Paldau/Landslides/Landslides_Paldau_02_18.shp"
path_rnd_poi = "Daten/Paldau/Samples/randomPoints.rds"

# ouput path
path_to_out = "Daten/Paldau/Samples"
name_mask = "maskPaldau"
name_random_values = out_names[i]

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

# uncomment to create mask

# dim = dim(rasStack)
# r = raster(ncol= dim[2], nrow=dim[1])
# extent(r) = extent(rasStack)
# r
# mask = rasterize(lanslde_poly, r)
#
# # change value where is no polygon to 2
# mask[is.na(mask[])] = 2
# # change values where a polygon is to NA
# mask[mask == 1] = NA
# plot(mask)
# # change twos back to ones. In fact this is not necessary
# mask[mask == 2] = 1
#
#
# crs(mask) = crs(rasStack)
#
# # write mask to disk
# writeRaster(mask, filename=file.path(path_to_out, name_mask),
#             format = "GTiff", overwrite = TRUE)

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

ext = extract(rasStack, rnd_poi)

samples_df = as.data.frame(t(ext))

saveRDS(samples_df, paste0(path_to_out,"/", name_random_values,".rds"))
}
