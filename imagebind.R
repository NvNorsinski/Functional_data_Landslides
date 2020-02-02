rm(list = ls(all = TRUE))
library(raster)
library(rgdal)



path = "Daten/Paldau/cutout/"
source_img = raster("Daten/Paldau/Outputs/prob_map_glm.tif")
source_img

crs = crs(source_img)
ext = extent(source_img)
res = res(source_img)

files = list.files(path = path, pattern = ".rds")
files

# dimensions of image and chunck
x = 10464
y = 300

dimension = c(x, y)
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

# number of last file in the brackets
leng = length(files)
leng
img = readRDS(paste0(path,files[leng]))

img = na.omit(img)
img = as.matrix(img$fit)

rest = (length(img[,1]))/x
dim(img) = c(x, rest)
img = t(img)

image_comp = rbind(image_comp, img)

dim(image_comp)
r = raster(image_comp)

extent(r) = ext
#res(r) = res
crs(r) = crs

plot(r)
writeRaster(r, "Daten/Paldau/Outputs/prob_map_fregre.glm_cut",  format="GTiff", overwrite=TRUE)
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

img = readRDS(paste0(path,files[leng]))
img = na.omit(img)
img = as.matrix(img$fit)
dim(img) = c(x, rest)
img = t(img)

image_comp_err = rbind(image_comp_err, img)

dim(image_comp_err)
r = raster(image_comp_err)
extent(r) = ext
crs(r) = crs
plot(r)
r

writeRaster(r, "Daten/Paldau/Outputs/prob_map_fregre.glm_error_cut",  format="GTiff", overwrite=TRUE)
