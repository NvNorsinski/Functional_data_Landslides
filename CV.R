# This script performs cross validation for funktional regression models
# Author Nils von Norsinski
rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)
library(ROCR)
library(foreach)
library(doParallel)

cl <- makeCluster(2)
registerDoParallel(cl, cores = 2)


# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
#dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")

geology = readRDS(file = "Daten/Paldau/Samples/geology.rds")
# categorical variables must be of type factor.
# Not a dataframe containing a factor variable
geology = geology$geology
genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")
tpi = readRDS(file = "Daten/Paldau/Samples/tpi.rds")

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")

response = as.factor(response)



leng = length(slope[1,])

lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

#-------------------------------------------------------------------------------




# create basis object
number_knots = 3
num_scenes = 50
min = 1

knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)
num_points = ncol(slope)

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by= 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)


polynomial_order = 4

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)

basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)

formula1 = response ~ slope + aspect_ns + aspect_ow + twi+ tpi + genCurvature +
  catchmant_area


k = 10
repetition = 5

auroc_list = 0
mmce_list = 0




# repeat cross validation
#for (j in 1:repetition){


results = foreach (j = 1:repetition, .combine = data.frame, .packages=c('fda.usc', "fda", "ROCR")) %dopar%{
#set.seed(50)
ii<-sample(1:leng)


slope_rand = slope[,ii]
aspect_ns_rand = aspect_ns[,ii]
aspect_ow_rand = aspect_ow[,ii]
genCurvature_rand = genCurvature[,ii]
catchmant_area_rand = catchmant_area[,ii]
tpi_rand = tpi[,ii]
twi_rand = twi[,ii]

response_rand = response[ii]

auroc_fold = 0
mmce_fold = 0


folds <- cut(seq(1, ncol(slope_rand)), breaks=k,labels=FALSE)


  for(i in 1:k){
  #i = 8
    print(i)
    #Segement your data by fold using the which() function
    testIndexes <- which(folds==i ,arr.ind=TRUE)

    #testData <- data_rand[, testIndexes]
   # trainData <- data_rand[-testIndexes, ]

    # train data

    response.train = response_rand[-testIndexes]
    slope.train = slope_rand[, -testIndexes ]
    aspect_ns.train = aspect_ns_rand[, -testIndexes ]
    aspect_ow.train = aspect_ow_rand[, -testIndexes ]
    genCurvature.train = genCurvature_rand[, -testIndexes ]
    catchmant_area.train = catchmant_area_rand[, -testIndexes ]
    tpi.train = tpi_rand[, -testIndexes ]
    twi.train = twi_rand[, -testIndexes ]



    asp_ns_basis.train = smooth.basis(tvec, aspect_ns.train, basis.x)
    asp_ow_basis.train = smooth.basis(tvec, aspect_ow.train, basis.x)
    slope_basis.train = smooth.basis(tvec, slope.train, basis.x)

    genCurv_basis.train = smooth.basis(tvec, genCurvature.train, basis.x)
    catch_basis.train = smooth.basis(tvec, catchmant_area.train, basis.x)
    tpi_basis.train = smooth.basis(tvec, tpi.train, basis.x)
    twi_basis.train = smooth.basis(tvec, twi.train, basis.x)



    slopefd.train = fdata(slope_basis.train$fd)
    aspect_nsfd.train = fdata(asp_ns_basis.train$fd)
    aspect_owfd.train = fdata(asp_ow_basis.train$fd)
    genCurvfd.train = fdata(genCurv_basis.train$fd)

    catchfd.train = fdata(catch_basis.train$fd)
    tpifd.train = fdata(tpi_basis.train$fd)
    twifd.train = fdata(twi_basis.train$fd)

    # replicate response variable for each variable included in the model
    #response.train = rep(response.train, number_variables)

    df.train = as.data.frame(response.train)
    #colnames(df.train)
    names(df.train) = "response"

    ldata.train = list(df = df.train, twi = twifd.train, slope = slopefd.train, tpi = tpifd.train,
                       aspect_ns = aspect_nsfd.train, aspect_ow = aspect_owfd.train,
                       genCurvature = genCurvfd.train,
                       catchmant_area = catchfd.train)



    # test data
    response.test = response_rand[testIndexes]
    slope.test = slope_rand[, testIndexes]
    aspect_ns.test = aspect_ns_rand[, testIndexes]
    aspect_ow.test = aspect_ow_rand[, testIndexes]
    genCurvature.test = genCurvature_rand[, testIndexes]
    catchmant_area.test = catchmant_area_rand[, testIndexes]
    tpi.test = tpi_rand[, testIndexes]
    twi.test = twi_rand[, testIndexes]



    asp_ns_basis.test = smooth.basis(tvec, aspect_ns.test, basis.x)
    asp_ow_basis.test = smooth.basis(tvec, aspect_ow.test, basis.x)
    slope_basis.test = smooth.basis(tvec, slope.test, basis.x)

    genCurv_basis.test = smooth.basis(tvec, genCurvature.test, basis.x)
    catch_basis.test = smooth.basis(tvec, catchmant_area.test, basis.x)
    tpi_basis.test = smooth.basis(tvec, tpi.test, basis.x)
    twi_basis.test = smooth.basis(tvec, twi.test, basis.x)



    slopefd.test = fdata(slope_basis.test$fd)
    aspect_nsfd.test = fdata(asp_ns_basis.test$fd)
    aspect_owfd.test = fdata(asp_ow_basis.test$fd)
    genCurvfd.test = fdata(genCurv_basis.test$fd)

    catchfd.test = fdata(catch_basis.test$fd)
    tpifd.test = fdata(tpi_basis.test$fd)
    twifd.test = fdata(twi_basis.test$fd)


    #response.test = rep(response.test, number_variables)

    df.test = as.data.frame(response.test)
    names(df.test) = "response"


    ldata.test = list(df = df.test, twi = twifd.test, slope = slopefd.test, tpi = tpifd.test,
                       aspect_ns = aspect_nsfd.test, aspect_ow = aspect_owfd.test,
                       genCurvature = genCurvfd.test,
                       catchmant_area = catchfd.test)


    # regression

    res.basis1 = fregre.glm(formula1, data = ldata.train, basis.x = basis.x,
                            basis.b = basis.b, family = binomial(link = "logit"))

    summary(res.basis1)

    par(mfrow=c(2,2))
    plot(res.basis1)
    par(mfrow=c(1,1))



    yfit.train = ifelse(res.basis1$fitted.values < 0.5, 0, 1)

    text.train = table(response.train, yfit.train)
    text.train

    pred = predict(res.basis1, newx = ldata.test, type = "response", se.fit = TRUE, level = 0.95)

    pred

    summ = summary(pred)
    #summ


    yfit.test = ifelse(pred$fit < 0.5, 0, 1)

    text.test = table(response.test, yfit.test)
    text.test


    # calc auroc

    predobj <- prediction(pred$fit, response.test)

    auroc <- performance(predobj , measure = "auc")@y.values[[1]]
    auroc
    auroc_fold[i] = auroc

    #tpr <- performance(predobj, measure = "tpr", "fpr")
    #plot(tpr, colorize = TRUE)

    # calc mmce
    mmce <- 1 - (sum(diag(text.test))/sum(text.test))
    mmce_fold[i] = mmce


  }

auroc_list[j] = mean(auroc_fold)
mmce_list[j] = mean(mmce_fold)

aurocn = mean(auroc_fold)
mmcen = mean(mmce_fold)
data.frame(auroc = aurocn, mmce = mmcen)

#print(auroc_list)
#print(mean(auroc_list))
#print(sd(auroc_list))
}
parallel::stopCluster(cl)

results

results = t(results)
index = seq(1,repetition * 2, by = 2)


results = data.frame(auroc = results[index], mmce = results[index + 1])
