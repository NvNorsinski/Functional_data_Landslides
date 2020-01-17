rm(list = ls(all = TRUE))
library(mlr)
library(ROCR)
library(fda.usc)

library("parallelMap")
parallelStartSocket(2)

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

list_nam = c("slope", "aspect_ns", "aspect_ow", "genCurvature", "catchmant_area", "tpi", "twi")


# predict-----------------------------------------------------------------------



#task = makeRegrTask(id = "logi", data = dat, target = "response")
response = as.factor(response)
dat = cbind(response, slope, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi)

task = makeClassifTask(id = "logi", data = dat, target = "response", coordinates = poi)

# regr.glm mit family binomial geht nicht
regr.lrn = makeLearner("classif.logreg")

regr.lrn2 = makeLearner("classif.logreg", predict.type = "response")
getLearnerParamSet(regr.lrn)


rdesc = makeResampleDesc("SpRepCV",  fold = 5, reps = 10)
rdesc

mod = train(regr.lrn2, task)
regmod = getLearnerModel(mod)

r = resample(regr.lrn, task, rdesc, measures = list(mmce, fpr, fnr, timetrain))
r$measures.test

r

# error rate
r$aggr

#pred1 = predict(mod, task = task)
#df = generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
#plotROCCurves(df)

parallelStop()

#predict(regmod, dat, type = "response" )

saveRDS(regmod, "Daten/Paldau/Outputs/glm_model.rds")
