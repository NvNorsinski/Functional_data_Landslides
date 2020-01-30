rm(list = ls(all = TRUE))
library(raster)
library(rgdal)

# resolution auf 1 m eter

path = "Daten/Paldau/data_map_fregre/"
source_img = raster("Daten/Paldau/Outputs/prob_map_glm.tif")
source_img

crs = crs(source_img)
ext = extent(source_img)
res = res(source_img)

files = list.files(path = path, pattern = ".rds")
files

dimension = c(10464, 350)
image_comp = 0


# create probability map--------------------------------------------------------
for (k in files) {
  print(k)
  img = readRDS(paste0(path,k))
  img = as.matrix(img$fit)
  dim(img) = dimension
  img = t(img)

  image_comp = rbind(image_comp, img)

}

img = readRDS(paste0(path,files[25]))

img = na.omit(img)
img = as.matrix(img$fit)
dim(img) = c(10464, 119)
img = t(img)

image_comp = rbind(image_comp, img)

dim(image_comp)
r = raster(image_comp)

extent(r) = ext
#res(r) = res
crs(r) = crs

writeRaster(r, "Daten/Paldau/Outputs/prob_map_fregre.glm",  format="GTiff", overwrite=TRUE)
#create error map---------------------------------------------------------------
image_comp_err = 0
for (k in files) {
  print(k)
  img = readRDS(paste0(path,k))
  img = as.matrix(img$se.fit)
  dim(img) = dimension
  img = t(img)

  image_comp_err = rbind(image_comp_err, img)

}

img = readRDS(paste0(path,files[25]))
img = na.omit(img)
img = as.matrix(img$fit)
dim(img) = c(10464, 119)
img = t(img)

image_comp_err = rbind(image_comp_err, img)

dim(image_comp_err)
r = raster(image_comp_err)
extent(r) = ext
crs(r) = crs
plot(r)
r

writeRaster(r, "Daten/Paldau/Outputs/prob_map_fregre.glm_error",  format="GTiff", overwrite=TRUE)
