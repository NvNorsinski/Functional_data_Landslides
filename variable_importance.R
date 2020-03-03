# Author Nils von Norsinski
# perform GLM estimation for non functional data
rm(list = ls(all = TRUE))
library(mlr)
library(ROCR)
library(fda.usc)
library(ggplot2)
library(RColorBrewer)

library("parallelMap")
parallelStartSocket(3)

# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")
#response = as.factor(response)

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
slope = t(slope)


#dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
#dgm = t(dgm)

aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ns = t(aspect_ns)


aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")
aspect_ow = t(aspect_ow)


geology = readRDS(file = "Daten/Paldau/Samples/geology.rds")
# categorical variables must be of type factor.
# Not a dataframe containing a factor variable
geology = geology$geology

genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
genCurvature = t(genCurvature)


catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")
catchmant_area = t(catchmant_area)




twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")
twi = t(twi)



poi = readRDS(file = "Daten/Paldau/Samples/geometry.rds")

leng_list = length(slope[1,])

list_nam = c("s", "a_ns", "a_ow", "gc",
             "ca", "twi")

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)
tvec
rep_names = rep(list_nam, times = 1, each = 22)
rep_names = paste0(rep_names, tvec)
rep_names
# predict-----------------------------------------------------------------------


#task = makeRegrTask(id = "logi", data = dat, target = "response")
response = as.factor(response)
dat = cbind(slope, aspect_ns, aspect_ow, genCurvature, catchmant_area, twi)
dat = as.data.frame(dat)
colnames(dat) = rep_names
dat = cbind(response, dat)
#dat = readRDS("Daten/Paldau/Samples/lsd_pointVal.rds")
dat$response = as.factor(dat$response)

task = makeClassifTask(id = "logi", data = dat, target = "response",
                       coordinates = poi)

# regr.glm mit family binomial geht nicht
# use response for model estimation
regr.lrn = makeLearner("classif.logreg",  predict.type = "response")

# use prob for variable importance
regr.lrn2 = makeLearner("classif.logreg", predict.type = "prob")
getLearnerParamSet(regr.lrn)


rdesc = makeResampleDesc("SpRepCV",  fold = 5, reps = 10)
rdesc

mod = train(regr.lrn2, task)
regmod = getLearnerModel(mod)

r = resample(regr.lrn2, task, rdesc, measures = list(auc, fpr, fnr, timetrain))
r$measures.test

r

#predict(regmod, dat, type = "response" )

#saveRDS(regmod, "Daten/Paldau/Outputs/glm_model.rds")

# error rate
r$aggr

#---variable importance and auroc-----------------------------------------------
regr.lrn2 = makeLearner("classif.logreg", predict.type = "prob")
getLearnerParamSet(regr.lrn)


rdesc = makeResampleDesc("SpRepCV",  fold = 5, reps = 10)
rdesc

mod = train(regr.lrn2, task)

regmod = getLearnerModel(mod)

r = resample(regr.lrn, task, rdesc, measures = list(mmce, fpr, fnr, timetrain))
r$measures.test

pred1 = predict(mod, task = task)
df = generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
plotROCCurves(df)

auroc = mlr::performance(pred1, mlr::auc)
auroc

fv = generateFilterValuesData(task, method = "auc")
fv

aucdat = fv$data
aucdat = aucdat[,c(-2,-3)]

tvec
sm_name = rep(tvec, times = 22)
sm_name = paste0(sm_name)
facorder = paste0(tvec)
facorder
fac_color = rep(list_nam, each = 22)
fac_color

aucdat= cbind(aucdat,sm_name, fac_color)


p = ggplot(aucdat)+
  geom_point(size = 3, aes(sm_name, value, label = name, colour = fac_color))+
  scale_x_discrete(limits=c(facorder))+
  #geom_text(aes(label=name), hjust=0, vjust=1)+
  xlab("Radius of moving window [m]") +
  ylab("AUROC") +
  theme_minimal()+
  labs(color = "Variables") +

  scale_color_brewer(palette = "Paired", labels = c("Aspect NS", "Aspect EW",
                              "Catchmant Area", "General Curvature",
                              "Slope", "TWI"))

p

ggsave("variables_auroc.png", plot = p, device = "png", dpi = 300, height =15,
       units = "cm", type = "cairo-png")



plotFilterValues(fv) +
  ggpubr::theme_pubr()

#-------------------------------------------------------------------------------

fctrl = makeFeatSelControlSequential(method = "sfs")

rdesc = makeResampleDesc("SpRepCV",  fold = 5, reps = 10)

sfeats = selectFeatures(learner = regr.lrn2, task = task, resampling = rdesc,
                        control = fctrl, show.info = FALSE, measures = list(auc))
#sfeats

#sfeats$x
#sfeats$y
analyzeFeatSelResult(sfeats)

parallelStop()
