rm(list = ls(all = TRUE))
library(raster)
model = readRDS("Daten/Paldau/Outputs/glm_model.rds")


model$terms
path_to_images = "C:/Users/Nils-Laptop/Documents/Master"

fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE, recursive = TRUE)
fs

rasStack = stack(fs)
rasStack


image = predict(rasStack, model,type = "response",  progress='text')

img_cat = image

img_cat[img_cat <= 0.25] = 0.25
img_cat[img_cat <= 0.5 & img_cat > 0.25 ] = 0.5
img_cat[img_cat <= 0.75 & img_cat > 0.5 ] = 0.75
img_cat[img_cat <= 0.9 & img_cat > 0.75 ] = 0.9
img_cat[img_cat <= 1 & img_cat > 0.9 ] = 1


plot(img_cat)

plot(image)


writeRaster(image, "Daten/Paldau/Outputs/prob_map_glm",  format="GTiff", overwrite=TRUE)

writeRaster(img_cat, "Daten/Paldau/Outputs/prob_map__categories_glm",  format="GTiff", overwrite=TRUE)

image


r <- raster(ncol=10, nrow=10)
values(r) <- 1:ncell(r)

plot(r)

r[r < 60  & r > 40] <- 50

plot(r)

r[8,]
