rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)


# read data
slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect = readRDS(file = "Daten/Paldau/Samples/aspect.rds")
response = readRDS(file = "Daten/Paldau/Samples/predict.rds")


lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

num_observations = nrow(dgm)
min = 1
number_knots = 4
num_points = ncol(dgm)
tvec = seq(from = 1, to = num_observations, by = 1)

rangeval = c(min, num_observations)
knotvec = lseq(from = min, to = num_observations, length.out = number_knots)

polynomial_order = 4
dgm_basis = create.bspline.basis(rangeval, norder = polynomial_order)
dgmsb = smooth.basis(tvec, dgm, dgm_basis)
aspsb = smooth.basis(tvec, aspect, dgm_basis)
#smoothfd = dgmsb$fd
#smoothasp = aspsb$fd

plot(dgm_basis)
# Low-Dimensional Regression Coefficient Function beta

dgmlist = vector("list", 3)
dgmlist[[1]] = rep(1, num_points)
dgmlist[[2]] = dgmsb$fd
dgmlist[[3]] = aspsb$fd

beta_basis = create.bspline.basis(rangeval, norder = polynomial_order)
const_basis = create.constant.basis(rangeval)

betalist = vector("list", 3)
betalist[[1]] = const_basis
betalist[[2]] = beta_basis
betalist[[3]] = beta_basis


fRegressList = fRegress(response, dgmlist, betalist)


betaestlist = fRegressList$betaestlist
dgmbetafd = betaestlist[[2]]$fd
plot(dgmbetafd, xlab="moving window size (not at the moment!!!!)",
     ylab="Beta for dgm")

intercept = coef(betaestlist[[1]])
intercept


# quality of fit
# extract fittet values
response1 = fRegressList$yhatfdobj
# calc residuals
response_precres = response - response1
# average of residuals
SSE1.1 = sum(response_precres**2)
SSE1.1
# error sums
SSE0 = sum((response - mean(response))**2)
SSE0
# total sum of squares
SSE1 = sum((response - response1)**2)
SSE1

# squared multiple correlation - how well can a varialbe be response using linear
# function
# 0 - 1 higher the better

RSQ1 = (SSE0 - SSE1.1)/SSE0
RSQ1

# 5 count of groups - nbasis ?
# 5 -> m - number of restrictions,
# 29 -> n-k - k number independent variables 35 - 5 = 30
# 29 weil n-1
Fratio1 = ((SSE0-SSE1)/(number_knots + 2))/
  (SSE1/(num_points - ((number_knots + 2)-1)))
Fratio1
#
# # ridge regression
# loglam = seq(0,30,5)
# nlam = length(loglam)
# SSE.CV = matrix(0,nlam,1)
# for (ilam in 1:nlam) {
#   print(ilam)
#   lambda = 10**loglam[ilam]
#   betalisti = betalist
#   betafdPar2 = betalisti[[2]]
#   betafdPar2$lambda = lambda
#   betalisti[[2]] = betafdPar2
#   fRegi = fRegress.CV(response, dgmlist, betalisti)
#   SSE.CV[ilam] = fRegi$SSE.CV
# }
#
# plot(loglam, SSE.CV)

