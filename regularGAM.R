rm(list = ls(all = TRUE))

library(ROCR)
library(sperrorest)

library(mgcv)



# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")
#response = as.factor(response)


slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
slope = t(slope)
slope = data.frame(slope[,1])

#dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
#dgm = t(dgm)

aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ns = t(aspect_ns)
aspect_ns = data.frame(aspect_ns[,1])

aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")
aspect_ow = t(aspect_ow)
aspect_ow = data.frame(aspect_ow[,1])

geology = readRDS(file = "Daten/Paldau/Samples/geology.rds")
# categorical variables must be of type factor.
# Not a dataframe containing a factor variable
geology = geology$geology

genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
genCurvature = t(genCurvature)
genCurvature = data.frame(genCurvature[,1])

catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")
catchmant_area = t(catchmant_area)
catchmant_area = data.frame(catchmant_area[,1])

tpi = readRDS(file = "Daten/Paldau/Samples/tpi.rds")
tpi = t(tpi)
tpi = data.frame(tpi[,1])

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")
twi = t(twi)
twi = data.frame(twi[,1])


poi = readRDS(file = "Daten/Paldau/Samples/geometry.rds")

leng_list = length(slope[1,])

list_nam = c("response", "slope", "aspect_ns", "aspect_ow", "genCurvature", "catchmant_area", "tpi", "twi", "x","y")

response = as.factor(response)
dat = cbind(response, slope, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi, poi)
names(dat) = list_nam
# ------------------------------------------------------------------------------

par_args <- list(par_mode = "foreach", par_units = NULL, par_option = NULL)


df = 10

formula = response ~ s(slope, k = df)+ s(aspect_ns, k = df)+ s(aspect_ow, k = df)+
  s(genCurvature, k = df)+ s(catchmant_area, k = df)+
  s(tpi, k = df)+ s(twi, k = df)

fo <- response ~ slope + aspect_ns + aspect_ow + genCurvature + catchmant_area + tpi + twi

res <- mgcv::gam(formula = formula, data = dat, family = binomial())
res


pred <- predict(res, dat)


yfit.train = ifelse(res$fitted.values < 0.5, 0, 1)

text.train = table(response, yfit.train)
text.train





my.gam <- function(formula, data, family = binomial, k = 4) {
  response.name <- as.character(formula)[2]
  predictor.names <- labels(terms(formula))
  categorical <- sapply(data[,predictor.names], is.logical) |
    sapply(data[,predictor.names], is.factor)
  formula <- paste(response.name, "~",
                   paste(predictor.names[categorical], collapse = "+"),
                   paste("s(", predictor.names[!categorical], ", k=", k, ")", collapse = "+"))
  formula <- as.formula(formula)
  fit <- gam(formula, data, family = family)
  return(fit)
}


res <- sperrorest(formula = fo, data = dat, coords = c("x","y"),
                  model_fun = my.gam,
                  pred_args = list(type="response"),
                  smp_fun = partition_kmeans, smp_args = list(repetition=1:10, nfold=5),
                  par_args = par_args)
summary(res$error_rep)[c("train_auroc","test_auroc"),1:2]
# Degree of overfitting (the more negative, the worse):
diff( summary(res$error_rep)[c("train_auroc","test_auroc"),"mean"] )

