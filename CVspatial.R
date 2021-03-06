# This script performs cross validation for funktional regression models
# Author Nils von Norsinski
rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)
library(ROCR)
library(foreach)
library(doParallel)
library(sperrorest)

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

poi = readRDS(file = "Daten/Paldau/Samples/geometry.rds")

response = as.factor(response)

# k fold variable of crossvalidation
k = 5
# repetitions of cross validation
repetition = 2


leng = length(slope[1,])

lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

regress_and_error = function(response, slope, aspect_ow, aspect_ns, genCurvature, catchmant_area,
                             tpi, twi, resamp, i, j, basis.x, basis.b, tvec, formula1){


  # train data
  # indices to access the indices of the spatial cross vlidation in resamp object
  # repetition = j, fold = i, train(0)/test(1)

  train_idx= resamp[[j]][[i]]$train
  test_idx= resamp[[j]][[i]]$test

  response.train = response[train_idx]
  slope.train = slope[, train_idx ]
  aspect_ns.train = aspect_ns[, train_idx ]
  aspect_ow.train = aspect_ow[, train_idx ]
  genCurvature.train = genCurvature[, train_idx ]
  catchmant_area.train = catchmant_area[, train_idx ]
  tpi.train = tpi[, train_idx ]
  twi.train = twi[, train_idx ]

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



  df.train = as.data.frame(response.train)

  names(df.train) = "response"

  ldata.train = list(df = df.train, twi = twifd.train, slope = slopefd.train, tpi = tpifd.train,
                     aspect_ns = aspect_nsfd.train, aspect_ow = aspect_owfd.train,
                     genCurvature = genCurvfd.train,
                     catchmant_area = catchfd.train)



  # test data
  response.test = response[test_idx]
  slope.test = slope[, test_idx]
  aspect_ns.test = aspect_ns[, test_idx]
  aspect_ow.test = aspect_ow[, test_idx]
  genCurvature.test = genCurvature[, test_idx]
  catchmant_area.test = catchmant_area[, test_idx]
  tpi.test = tpi[, test_idx]
  twi.test = twi[, test_idx]


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




  df.test = as.data.frame(response.test)
  names(df.test) = "response"


  ldata.test = list(df = df.test, twi = twifd.test, slope = slopefd.test, tpi = tpifd.test,
                    aspect_ns = aspect_nsfd.test, aspect_ow = aspect_owfd.test,
                    genCurvature = genCurvfd.test,
                    catchmant_area = catchfd.test)


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

  summ = summary(pred)
  #summ

  yfit.test = ifelse(pred$fit < 0.5, 0, 1)
  text.test = table(response.test, yfit.test)
  text.test


  # calc error measures

  predobj <- prediction(pred$fit, response.test)

  auroc <- performance(predobj , measure = "auc")@y.values[[1]]
  rmse <- performance(predobj , measure = "rmse")@y.values[[1]]
  mmce <- 1 - (sum(diag(text.test))/sum(text.test))


  return(list(auroc, rmse, mmce))


}

# create basisobjects and formulas----------------------------------------------
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


resamp = partition_kmeans(poi, nfold = k, repetition = repetition, coords = c("x", "y"), seed1 = 1000)
#-------------------------------------------------------------------------------
# create list to store results of each repetition
#auroc_list = 0
#mmce_list = 0
#rmse_list = 0

#  cross validation sequenzial
#for (j in 1:repetition){

# cross validation parallel

CV = function(basis.x, basis.b, slope, aspect_ns, aspect_ow, genCurvature, k, poi, response, resamp,
              tpi, twi, tvec, catchmant_area, formula1, regress_and_error, repetition){

results = foreach (j = 1:repetition, .combine = data.frame, .packages=c('fda.usc', "fda", "ROCR")) %dopar%{



#-------------------------------------------------------------------------------
# create lists to store results of each run k
auroc_fold = 0
mmce_fold = 0
rmse_fold = 0


  for(i in 1:k){
    # indices to access the indices of the spatial cross vlidation in resamp object
    # repetition = j, fold = i, train(0)/test(1)

  #i = 8
    errors = regress_and_error(slope = slope, aspect_ow = aspect_ow, aspect_ns = aspect_ns,
                               response = response, genCurvature, twi = twi, tpi = tpi,
                               catchmant_area = catchmant_area, i = i, j = j, resamp = resamp, basis.x = basis.x,
                               basis.b = basis.b, tvec = tvec, formula1 = formula1)
    auroc_fold[i] = errors[[1]]
    rmse_fold[i] = errors[[2]]
    mmce_fold[i] = errors[[3]]


  }



aurocn = mean(auroc_fold)
sd_auroc = sd(auroc_fold)
#print(sd_auroc)
mmcen = mean(mmce_fold)
rmsen = mean(rmse_fold)

data.frame(auroc = aurocn, mmce = mmcen, rmse = rmsen, sd_auroc = sd_auroc)


}
return(results)

}


results = CV(basis.x, basis.b, slope, aspect_ns, aspect_ow, genCurvature, k, poi, response, resamp,
        tpi, twi, tvec, catchmant_area, formula1, regress_and_error, repetition)



parallel::stopCluster(cl)

# create index to subset the data frame and store results columwise
results = t(results)
index = seq(1, repetition * 4, by = 4)


results = data.frame(auroc = results[index], mmce = results[index + 1], rmse = results[index + 2], sd_auroc = results[index+3])

#results = colMeans(results)
#results

