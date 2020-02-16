# Author: Nils von Norsinski
# predict landslide pobability map

rm(list = ls(all = TRUE))
library(raster)
library(mgcv)

# rasper predict glm
model = readRDS("Daten/Paldau/Outputs/glm_model.rds")


# file contains original unsmoothed images of all variables
# e.q. slop, aspect,...
path_to_images = "Daten/Paldau/nonfunctional/glm/"

fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE,
                recursive = TRUE)
fs

rasStack = stack(fs)
rasStack


image = predict(rasStack, model,type = "response", se.fit = TRUE,
                progress='text')


image


# return se???
# predfun <- function(model, data) {
#   v <- predict(model, data,type = "response", se.fit=TRUE)
#   cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
# }
#
#
# r2 <- predict(rasStack, model, fun=predfun, index=1:2)
#
# plot(r2$layer.2)



img_cat = image

img_cat[img_cat <= 0.25] = 0.25
img_cat[img_cat <= 0.5 & img_cat > 0.25 ] = 0.5
img_cat[img_cat <= 0.75 & img_cat > 0.5 ] = 0.75
img_cat[img_cat <= 0.9 & img_cat > 0.75 ] = 0.9
img_cat[img_cat <= 1 & img_cat > 0.9 ] = 1


plot(img_cat)

plot(image)


#writeRaster(r2$layer.2, "Daten/Paldau/Outputs/prob_map_glm2",
#format="GTiff", overwrite=TRUE)

writeRaster(img_cat, "Daten/Paldau/Outputs/prob_map__categories_glm",
            format="GTiff", overwrite=TRUE)

image

# raster predict gam------------------------------------------------------------

model = readRDS("Daten/Paldau/Outputs/model_gam_r.rds")
model$model
path_to_images = "Daten/Paldau/nonfunctional/gam/"
# variable names are different than in glm model, so renaming images is
# neccessary


fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE,
                recursive = TRUE)
fs

rasStack = stack(fs)
rasStack



image = predict(rasStack, model,type = "response", se.fit = TRUE,
                progress='text')





img_cat = image

img_cat[img_cat <= 0.25] = 0.25
img_cat[img_cat <= 0.5 & img_cat > 0.25 ] = 0.5
img_cat[img_cat <= 0.75 & img_cat > 0.5 ] = 0.75
img_cat[img_cat <= 0.9 & img_cat > 0.75 ] = 0.9
img_cat[img_cat <= 1 & img_cat > 0.9 ] = 1


plot(img_cat)

plot(image)

writeRaster(img_cat, "Daten/Paldau/Outputs/prob_map_categories_gam",
            format="GTiff", overwrite=TRUE)

writeRaster(image, "Daten/Paldau/Outputs/prob_map_gam",
            format="GTiff", overwrite=TRUE)

