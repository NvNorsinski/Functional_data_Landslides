# Author: Nils von Norsinski
# extract the values from a shape file at position of points
# and extraxt non landslied values from geology raster file

# not used
rm(list = ls(all = TRUE))
library(sf)
library(raster)
library(dplyr)

# input points

path_to_points = "Daten/Paldau/Landslides/Cliped_landslide_points.shp"
# areas of landslides are masked out with value 0 in raster
path_masked_geology = "Daten/Paldau/Geology/Geologie_GIS_Paldau_raster_masked.tif"
# points wher landsliedes are
points = st_read(path_to_points)

# input folder of shape
path_to_shape = "Daten/Paldau/Geology/Geologie_GIS_Paldau.shp"
shape = st_read(path_to_shape)
path_random_poi = "Daten/Paldau/Samples/randomPoints.rds"
#plot(shape)

# output folder
name_yes_lsd = "geology_pointVal"
name_no_lsd = "geology_nolsd_pointVal"
path_to_out = "Daten/Paldau/Samples/"
samples_name = "randomPoints"


# extract values where landslides occur from shape file
landlide_yes = st_join(points, shape, join = st_intersects)
landlide_yes

#plot(points)

saveRDS(landlide_yes, paste0(path_to_out, name_yes_lsd,".rds"))


# extract values of no landslide points
# generate random points
sample = st_sample(shape, 420, "random")

#plot(shape)
# coordinates as sf object
sample_sf = st_sf(data.frame(a=1:2, geom=sample))

rasStack = stack(path_masked_geology)
rasValue = extract(rasStack, sample_sf)
rasValue

combinePointValue = cbind(sample_sf, rasValue)

# remove samples with value 0. These are Landslides
combinePointValue = combinePointValue[
  which(combinePointValue$Geologie_GIS_Paldau_raster_masked != 0), ]


combinePointValue$ID = seq.int(nrow(combinePointValue))

# save random points to disk
newpos = combinePointValue$geometry

sample = do.call(rbind, st_geometry(newpos)) %>%
  as.data.frame %>% setNames(c("x","y"))

saveRDS(sample, paste0(path_to_out, samples_name,".rds"))


# match numeric values for geology in raster with the corresponding values in
# shape file

# geology and points to data frame
st_geometry(shape) = NULL
st_geometry(combinePointValue) = NULL


no_landslide = left_join(combinePointValue, shape,
                 by = c("Geologie_GIS_Paldau_raster_masked" = "LEGNUMMER"))


# remove duplicate values and save non Landslide points to disk
no_landslide = no_landslide[!duplicated(no_landslide[,c('ID')]),]

saveRDS(no_landslide, paste0(path_to_out, name_no_lsd,".rds"))
