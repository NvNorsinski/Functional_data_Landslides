# This script performs cross validation for funktional regression models
# Author Nils von Norsinski
rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)
library(ggplot2)
library(rasterVis)
library(rgdal)

library(sf)
library(raster)
library(foreach)
library(doParallel)
#-------------------------------------------------------------------------------
prepath = "C:/Users/Nils-Laptop/Documents/Master"
path_to_images = "Daten/Paldau/Parameters/"

img = brick(paste0(prepath, "/Filtered_images_tpi/Filtered_images_tpitpi_120.tif"))
img

model = readRDS("Daten/Paldau/Samples/model.rds" )
#-------------------------------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

num_scenes = 50
min = 1

number_knots = 6
polynomial_order = 10




tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)



knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)



basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)




chunk_size = 1024

resolution = c(8517, 10464)
resolution/chunk_size

res = resolution %/% chunk_size
res
rest = resolution %% chunk_size

chunk_x = c(rep(chunk_size, res[1]), rest[1])
chunk_x =

chunk_y = c(rep(chunk_size, res[2]), rest[2])

chunk_x_idx = 0

# create indices
for (i in 1:length(chunk_x)) {
  chunk_x_idx[i] = sum(chunk_x[1:i])

}

chunk_y_idx = 0

# create indices
for (i in 1:length(chunk_y)) {
  chunk_y_idx[i] = sum(chunk_y[1:i])

}


fs = list.files(path=prepath, pattern = "tif$", full.names = TRUE, recursive = TRUE)
fs




rasStack = stack(fs)




dat = getValuesBlock(rasStack, 2, 1, 2, 10464)
dat

dat = as.data.frame(dat)


l = split.default(dat,rep(1:7, each = 22) )
names = c("aspect_ns", "aspect_ow", "catchmant_area", "genCurvature", "slope", "tpi", "twi")
names(l) = names

data = list()

j = 1



for (i in l) {
  i = t(i)
  is = smooth.basis(tvec, i, basis.x)
  data[[j]] = fdata(is$fd)

  j = j+1
}



data2 = list(aspect_ns = data[[1]], aspect_ow = data[[2]], catchmant_area = data[[3]], genCurvature = data[[4]],
             slope = data[[5]], tpi = data[[6]], twi = data[[7]])

datadata = data2$aspect_ns
datadata


pred = predict(model, newx = data2, type = "response", se.fit = TRUE, level = 0.95)
pred

yfit.test = ifelse(pred < 0.5, 0, 1)
yfit.test

pred = as.data.frame(pred)
summary(pred)
ss = as.data.frame(pred)
