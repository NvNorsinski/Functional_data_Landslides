rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)


# read data
slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect = readRDS(file = "Daten/Paldau/Samples/aspect_sincos.rds")
response = readRDS(file = "Daten/Paldau/Samples/response.rds")
geology = readRDS(file = "Daten/Paldau/Samples/geology.rds")


# it is necessary to repeat the response vector for each included variable
number_variables = 3

# preparation of data-----------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

# create basis object
number_knots = 4
num_observations = nrow(dgm)
min = 1

knotvec = lseq(from = min, to = num_observations, length.out = number_knots)
num_points = ncol(dgm)
tvec = seq(from = 1, to = num_observations, by = 1)

rangeval = c(min, num_observations)

# cubic bsplines -> 4
polynomial_order = 4
basis.x = create.bspline.basis(rangeval, norder = polynomial_order)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order)
plot(basis.x)

dgm_basis = smooth.basis(tvec, dgm, basis.x)
asp_basis = smooth.basis(tvec, aspect, basis.x)
slope_basis = smooth.basis(tvec, slope, basis.x)

#plot(slope_basis)

# prepare data
# transposing dataframes is necessary
dgmfd = fdata(t(dgm))
slopefd = fdata(t(slope))

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)



# one variable------------------------------------------------------------------
res.basis0 = fregre.basis(dgmfd, y = response, basis.x = basis.x, basis.b = basis.b)

res.basis0
summary(res.basis0)
yfit = ifelse(res.basis0$fitted.values < 0.5, 0, 1)
table(response, yfit)



plot(res.basis0$beta.est)
plot(res.basis0$b.est)


#a.est -> intercept
#b.est -> beta
#beta.est -> functional beta values


# two functional variables------------------------------------------------------
ldata = list()
df = as.data.frame(response)

ldata = list(dgm = dgmfd, df = df, slope = slopefd)


formula = response ~ dgm + slope


res.basis0 = fregre.lm(formula, data = ldata, basis.x = basis.x, basis.b = basis.b)

res.basis0
summary(res.basis0)

yfit = ifelse(res.basis0$fitted.values < 0.5, 0, 1)
table(response, yfit)

par(mfrow=c(2,2))
plot(res.basis0)
par(mfrow=c(1,1))
plot(res.basis0$beta.l$dgm)
coeffs_dgm = res.basis0$beta.l$dgm$coefs
coeffs_slope = res.basis0$beta.l$slope$coefs



# two variables + non functional variable---------------------------------------
formula = response ~ dgm + slope + geology
ldata = list()

# categorical variables must be of type factor.
# Not a dataframe containing a factor variable
geology = geology$geology

#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = cbind(df, geology)
ldata = list(df = df, dgm = dgmfd, slope = slopefd)




res.basis1 = fregre.lm(formula, data = ldata, basis.x = basis.x, basis.b = basis.b)
res.basis1

summary(res.basis1)

yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)
table(response, yfit)

par(mfrow=c(2,2))
plot(res.basis1)
par(mfrow=c(1,1))

plot(res.basis1$assign)

res.basis1$coefficients


res.basis1$beta.l$dgm$coefs
plot(res.basis1$beta.l$dgm$coefs)

# pca---------------------------------------------------------------------------

# construct n dimensional array of slope-, dgm- etc. data for pca
dat = cbind(dgm, slope)

fdafd = smooth.basis(tvec, dat, basis.x)$fd


datfd = Data2fd(dat, argvals = 1:22, basisobj = basis.x)

fdat = fdata(datfd)
# replikate response variable
response1 = c(response, response)

# l -> index of elements to include
l = 1:3
res = fregre.pc(datfd, y = response1, l = l, basis.x = basis.x)
res

summary(res)
res$lambda



# criteria
lambda<- 10**seq(-2, 14, by = 1)

# penalizaton off
lambda = 0
res2 = fregre.pc.cv(datfd, y = response1, lambda = lambda, P=c(1,0,0), basis.x = basis.x, kmax = 3, criteria = "SIC")

res2

summary(res2)

plot(res2$fregre.pc$beta.est)
plot(res2$pc.opt)
res2$MSC.order
res2$PC.order



# ridge regression--------------------------------------------------------------

formula = response1 ~ dgm + slope
ldata = list()
# categorical variables must be of type factor.
# Not a dataframe containing a factor variable


#df = cbind(df, geology)
df = as.data.frame(response1)
ldata = list(df = df, dgm = dgmfd, slope = slopefd)




lambda<- 10**seq(-2, 14, by = 1)
lams = c(0,1)
lambda = as.list(lambda)

rn = P.penalty(tt = 1:22, P=c(0,0,1))
rn
res.basis1 = fregre.lm(formula, data = ldata, basis.x = basis.x)

r = fregre.glm(formula, data = ldata, basis.x = basis.x, family = "binomial")
r

res.basis1$rn
summary(res.basis1)
plot(res.basis1)




res.basis1 = fregre.basis.cv(datfd, y = response1, basis.x = basis.x, basis.b = basis.x,
                             type.basis = "bspline", lambda = lambda, type.CV = GCV.S,
                             par.CV = list(trim = 0.15))

res.basis1$gcv

summary(res.basis1)

yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)
table(response, yfit)

par(mfrow=c(2,2))
plot(res.basis1)
par(mfrow=c(1,1))


