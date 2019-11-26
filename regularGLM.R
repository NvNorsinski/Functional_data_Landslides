rm(list = ls(all = TRUE))

# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")
#response = as.factor(response)


slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
slope = t(slope)

dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
dgm = t(dgm)

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

tpi = readRDS(file = "Daten/Paldau/Samples/tpi.rds")
tpi = t(tpi)

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")
twi = t(twi)

leng_list = length(slope[1,])

list_nam = c("slope", "aspect_ns", "aspect_ow", "genCurvature", "catchmant_area", "tpi", "twi")


number_variables = 7

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

tvec = rep(tvec, number_variables)

nam = rep(list_nam, each = leng_list)

nam = paste0(nam,": ", tvec)
dat = cbind(slope, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi)
colnames(dat) = nam
dat = as.data.frame(dat)

dat = cbind(response, dat)
response = as.factor(dat$response)

colnames(dat) <- make.names(colnames(dat),unique = TRUE)


response_rep = rep(response, number_variables)



leng = length(slope[,1])

ii<-sample(1:leng,leng*0.8)

# train data
response.train = response[ii]

slope.train = slope[ii,]
dgm.train = dgm[ii,]
aspect_ns.train = aspect_ns[ii,]
aspect_ow.train = aspect_ow[ii,]


genCurvature.train = genCurvature[ii,]
catchmant_area.train = catchmant_area[ii,]
tpi.train = tpi[ii,]

twi.train = twi[ii,]

# test data
response.test = response[-ii]

slope.test = slope[-ii,]
dgm.test = dgm[-ii,]
aspect_ns.test = aspect_ns[-ii,]
aspect_ow.test = aspect_ow[-ii,]


genCurvature.test = genCurvature[-ii,]
catchmant_area.test = catchmant_area[-ii,]
tpi.test = tpi[-ii,]

twi.test = twi[-ii,]

geology.train = geology[ii]
geology.test = geology[-ii]


# replicate response variable for each variable included in the model


formula1 = response.train ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
                         catchmant_area + twi + geology

ldata.train = list(twi = twi.train, slope = slope.train, tpi = tpi.train,
              aspect_ns = aspect_ns.train, aspect_ow = aspect_ow.train,
              genCurvature = genCurvature.train,
              catchmant_area = catchmant_area.train,
              geology = geology.train)


fit = glm(formula1, family = binomial(link = "logit"), data = ldata.train)
fit$deviance

yfit = ifelse(fit$fitted.values < 0.5, 0, 1)

text = table(response.train, yfit)
text



# singularity error
# confidence intervall
#SE <- broom::tidy(fit)$std.error

#X <- model.matrix(fit)
#p <- fitted(fit)
#W <- diag(p*(1-p))


#V <- solve(t(X)%*%W%*%X)
#all.equal(vcov(fit), V)


#all.equal(SE, sqrt(diag(V)))


summary(fit)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))


# predict-----------------------------------------------------------------------



ldata.test = list(twi = twi.test, slope = slope.test, tpi = tpi.test,
                  aspect_ns = aspect_ns.test, aspect_ow = aspect_ow.test,
                  genCurvature = genCurvature.test,
                  catchmant_area = catchmant_area.test,
                  geology = geology.test)


pred = predict.glm(fit, ldata.test, type = "response" , se.fit = TRUE)

yfit = ifelse(pred$fit < 0.5, 0, 1)

text = table(response.test, yfit)
text


library(mlr)

#task = makeRegrTask(id = "logi", data = dat, target = "response")
dat$response = as.factor(dat$response)


task = makeClassifTask(id = "logi", data = dat, target = "response")

# regr.glm mit family binomial geht nicht
regr.lrn = makeLearner("classif.logreg")

regr.lrn2 = makeLearner("classif.logreg", predict.type = "prob")
getLearnerParamSet(regr.lrn)


mod = train(regr.lrn, task)
mod2 = train(regr.lrn2, task)
mod


rdesc = makeResampleDesc("RepCV",  fold = 5, reps = 10)
rdesc
r = resample(regr.lrn, task, rdesc, measures = list(mmce, fpr, fnr, timetrain))
r$measures.test

pred1 = predict(mod2, task = task)

df = generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
plotROCCurves(df)
