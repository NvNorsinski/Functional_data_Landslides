rm(list = ls(all = TRUE))
library(factoextra)

# read data
#response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
slope = t(slope)
#dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ns = t(aspect_ns)
aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")
aspect_ow = t(aspect_ow)


genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
genCurvature = t(genCurvature)
catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")
catchmant_area = t(catchmant_area)
tpi = readRDS(file = "Daten/Paldau/Samples/tpi.rds")
tpi = t(tpi)
# something is wrong with this dataset
twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")
twi = t(twi)


dat = cbind(slope, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi)



pca = princomp(dat, scale = TRUE)
pca
summary(pca)
plot(pca)


fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 100), title="")

