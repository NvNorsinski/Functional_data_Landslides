# Author: Nils von Norsinski
# extract the values from a raster at position of points
rm(list = ls(all = TRUE))
library(raster)
library(sf)
library(tidyverse)

# input data

files = c("Filtered_images_aspect_ns", "Filtered_images_aspect_ow",
          "Filtered_images_catchmant_area", "Filtered_images_dgm",
          "Filtered_images_generalCurvature", "Filtered_images_planCurvature",
          "Filtered_images_slope", "Filtered_images_tpi", "Filtered_images_twi")

out_names = c("grid_Paldau_aspect_ns", "grid_Paldau_aspect_ow",
              "grid_Paldau_catchmant_area", "grid_Paldau_dgm",
              "grid_Paldau_generalCurvature",
              "grid_Paldau_planCurvature", "grid_Paldau_slope",
              "grid_Paldau_tpi", "grid_Paldau_twi")

leng = length(files)
# for random sampling points
path_to_points = "Daten/Paldau/Landslides/Cliped_landslide_points.shp"
points = st_read(path_to_points)

# generate samples for landslide probability map
points= readRDS("Daten/Paldau/Samples/t.grid_small.rds")



# output folder
path_to_images = "Daten/Paldau/Parameters/"

path_to_out = "Daten/Paldau/Samples/"


tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

for (i in 1:leng) {
  i = 1

  fs = list.files(path=paste0(path_to_images,files[i]), pattern = "tif$", full.names = TRUE)
  fs
  rasStack = stack(fs)

    # extract the values from the raster. This may take a while
  rasValue = raster::extract(rasStack, points)
  rasValue

  combinePointValue = cbind(points, rasValue)
  combinePointValue = combinePointValue[rowSums(is.na(combinePointValue[ , ])) == 0, ]

  rasValue = rasValue[rowSums(is.na(rasValue[ , ])) == 0, ]


  ras_leng = length(rasValue[,1])
  pos_names = seq(1:ras_leng)
  pp = paste0("pos: ", pos_names)
  rownames(rasValue) = pp
  colnames(rasValue) = tvec
  rasValue = t(rasValue)

  saveRDS(rasValue, paste0(path_to_out, out_names[i],".rds"))


}


# deletes points with no pixel data
y = combinePointValue$geometry


grid_new = do.call(rbind, st_geometry(y)) %>%
  as_tibble() %>% setNames(c("x","y"))
saveRDS(grid_new, "Daten/Paldau/Samples/grid_new.rds")
