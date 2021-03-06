# Author Nils von Norsinski
# perform GLM estimation for non functional data
rm(list = ls(all = TRUE))
library(mlr)
library(ROCR)
library(fda.usc)

#library("parallelMap")
#parallelStartSocket(3)

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

list_nam = c("slope", "aspect_ns", "aspect_ow", "genCurvature",
             "catchmant_area", "tpi", "twi")


# predict-----------------------------------------------------------------------


#task = makeRegrTask(id = "logi", data = dat, target = "response")
response = as.factor(response)
dat = cbind(response, slope, aspect_ns, aspect_ow, genCurvature, catchmant_area,
            tpi, twi)
dat = readRDS("Daten/Paldau/Samples/lsd_pointVal.rds")
dat$response = as.factor(dat$response)

task = makeClassifTask(id = "logi", data = dat, target = "response",
                       coordinates = poi)

# regr.glm mit family binomial geht nicht
# use response for model estimation
regr.lrn = makeLearner("classif.logreg",  predict.type = "response")

# use prob for variable importance
regr.lrn2 = makeLearner("classif.logreg", predict.type = "prob")
getLearnerParamSet(regr.lrn)


rdesc = makeResampleDesc("SpRepCV",  fold = 5, reps = 100)
rdesc

mod = train(regr.lrn2, task)
regmod = getLearnerModel(mod)

regmod$fitted.values

r = resample(regr.lrn2, task, rdesc, measures = list(auc, fpr, fnr, timetrain))
r$measures.test

r$extract
r$pred$data$prob.0

mod$learner.model
plot(mod$learner.model)
mod$learner.model$coefficients

#png(filename="GLM_summary.png", width = 750, height = 750, type="cairo")

par(mfrow=c(2,2), oma = c(0, 1, 2, 0))

plot(mod$learner.model, cex.axis = 1.5 ,cex.lab = 1.5, lwd = 2, cex.sub = 2)




par(mfrow=c(1,1))
#dev.off()
library(tidyverse)

#saveRDS(r, "Daten/glm_auroc.rds")
boxplot(r$measures.test$auc)
predict(regmod, dat, type = "response" )
regmod$fitted.values
#saveRDS(regmod, "Daten/Paldau/Outputs/glm_model.rds")

# error rate
r$aggr

probabilities <- predict(mod$learner.model, type = "response")


# assumpion of linearity--------------------------------------------------------
mydata <- dat %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata  %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "Predictor.value", -logit)

labs <- c(`aspect_ns...1.` = "Aspect ns", `aspect_ow...1.` = "Aspect ew", `catchmant_area...1.` = "Catchment Area",
          `genCurvature...1.` = "General Curvature", `slope...1.` = "Slope", 	`twi...1.` = "TWI")


p = ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.2) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  ylab("Predictor values")+
  xlab("Logit")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))+
  facet_wrap(~predictors, scales = "free_y", labeller = as_labeller(labs))+
  theme(strip.text.x = element_text(size = 10))
p

ggsave("linearity.png", plot = p, device = "png", dpi = 350, height =10,
       units = "cm", type = "cairo-png")


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
