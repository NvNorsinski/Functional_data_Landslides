# extract the values from a shape file at position of points
rm(list = ls(all = TRUE))
library(sf)
library(raster)

# input points

path_to_points = "Daten/Paldau/Landslides/Paldau_source_point.shp"
points = st_read(path_to_points)

# input folder of shape
path_to_shape = "Daten/Paldau/Geology/Geologie_GIS_Paldau.shp"
shape = st_read(path_to_shape)
path_random_poi = "Daten/Paldau/Samples/randomPoints.rds"
#plot(shape)

# output folder
name = "geology_pointVal"
name_no_lsd = "geology_nolsd_pointVal"
path_to_out = "Daten/Paldau/Samples/"


# generate random samples of points in polygon


geology = st_join(points, shape, join = st_intersects)
geology

plot(points)

#saveRDS(geolgy, paste0(path_to_out, name,".rds"))

# sample random points with no landslide
#rnd_poi = readRDS(path_random_poi)
#rnd_poi = st_as_sf(rnd_poi, coords = c("x", "y"), crs = crs(geology))


sample = st_sample(shape, 400, "random")


sample_sf = st_sf(data.frame(a=1:2, geom=sample))

# convert sampled points to data frame
sample = do.call(rbind, st_geometry(sample)) %>%
  as.data.frame %>% setNames(c("x","y"))

saveRDS()

geology_no_lsd = st_join(sample_sf, shape, join = st_intersects)

saveRDS(geology_no_lsd, paste0(path_to_out, name,".rds"))
