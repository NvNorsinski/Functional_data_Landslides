# This script performs cross validation for funktional regression models
# Author Nils von Norsinski
rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)

library(raster)
library(rgdal)

#-------------------------------------------------------------------------------
prepath = "C:/Users/Nils-Laptop/Documents/Master/parameters"
path_to_images = "Parameters/"



model = readRDS("Daten/Paldau/Outputs/model.rds" )
model
#-------------------------------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

num_scenes = 50
min = 1

number_knots = 4
polynomial_order = 4




tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)



knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)



basis.x = create.bspline.basis(rangeval, norder = polynomial_order,
                               breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order,
                               breaks = knotvec)




chunk_size = 140

resolution = c(8517, 10464)

resolution/chunk_size

res = resolution %/% chunk_size
res

rest = resolution %% chunk_size
rest


chunk_x = c(rep(chunk_size, res[1]), rest[1])
chunk_x



chunk_x_idx = 0

# create indices
for (i in 1:length(chunk_x)) {
  chunk_x_idx[i] = sum(chunk_x[1:i])

}
chunk_x_idx =c(2, chunk_x_idx)
chunk_x_idx

fs = list.files(path=prepath, pattern = "tif$", full.names = TRUE,
                recursive = TRUE)
fs




rasStack = stack(fs)
pred_image = 0

for (k in 1:length(chunk_x_idx)) {
  print(k)



  dat = getValuesBlock(rasStack, chunk_x_idx[k], chunk_size, 2, 10464)


  dat = as.data.frame(dat)


  dat = split.default(dat, rep(1:6, each = 22))
  temp =  dat[[3]]
  temp = log10(temp[,])
  dat[[3]] = temp
  rm(temp)

  names = c("aspect_ns", "aspect_ow", "catchmant_area", "genCurvature",
            "slope", "twi")
  names(dat) = names

  data = list()

  j = 1



  for (i in dat) {
    i = t(i)
    is = smooth.basis(tvec, i, basis.x)
    data[[j]] = fdata(is$fd)


    j = j+1
  }
  rm(is, i,dat)



  data = list(aspect_ns = data[[1]], aspect_ow = data[[2]],
              catchmant_area = data[[3]], genCurvature = data[[4]],
               slope = data[[5]], twi = data[[6]])



  pred = predict(model, newx = data, type = "response", se.fit = TRUE,
                 level = 0.95)




  pred = as.data.frame(pred)


  saveRDS(pred, paste0("Daten/Paldau/imagecor/", 100+k, ".rds"))
  rm(data)


}




