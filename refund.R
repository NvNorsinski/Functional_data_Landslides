rm(list = ls(all = TRUE))
library(refund)
library(fda)
library(fda.usc)

# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
slopet = t(slope)
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
dgmt = t(dgm)
aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_nst = t(aspect_ns)
aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")
aspect_owt = t(aspect_ow)

geology = readRDS(file = "Daten/Paldau/Samples/geology.rds")
# categorical variables must be of type factor.
# Not a dataframe containing a factor variable
geology = geology$geology

genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
genCurvaturet = t(genCurvature)
catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")
catchmant_areat = t(catchmant_area)
tpi = readRDS(file = "Daten/Paldau/Samples/tpi.rds")
tpit = t(tpi)
# something is wrong with this dataset
twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")
twit = t(twi)

list_dat = list(slope, dgm, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi)
list_nam = c("slope", "dgm", "aspect_ns", "aspect_ow", "genCurvature", "catchmant_area", "tpi", "twi")
leng_list = length(list_dat)


# it is necessary to repeat the response vector for each included variable
number_variables = 8

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)

#response_rep <-as.factor(response_rep)




num_scenes = 50
min = 1

num_points = ncol(slope)

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)

#number_knots = seq(3:8)

# to high polynomial order leads to artefacts. keep order at 6 or below
# keep number of knots at 10 or below

# number of knots = i
# polynomial order = j

lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

i = 3
j = 3

# create basis object
number_knots = i


knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)


# cubic bsplines -> 4
polynomial_order = j

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)


dgm_basis = smooth.basis(tvec, dgm, basis.x)$fd
asp_ns_basis = smooth.basis(tvec, aspect_ns, basis.x)$fd
asp_ow_basis = smooth.basis(tvec, aspect_ow, basis.x)$fd
slope_basis = smooth.basis(tvec, slope, basis.x)$fd

genCurv_basis = smooth.basis(tvec, genCurvature, basis.x)$fd
catch_basis = smooth.basis(tvec, catchmant_area, basis.x)$fd
tpi_basis = smooth.basis(tvec, tpi, basis.x)$fd
twi_basis = smooth.basis(tvec, twi, basis.x)$fd




list_fd = list(slope_basis, dgm_basis, asp_ns_basis, asp_ow_basis, genCurv_basis, catch_basis, tpi_basis, twi_basis)

formula1 = response ~ af(slope_basis) +
  af(dgm_basis)+
  af(asp_ns_basis)+
  af(asp_ow_basis)+
  af(genCurv_basis)+
  af(catch_basis)+
  af(tpi_basis)+
  af(twi_basis)





formula1 = response ~ lf(slope_basis, argvals = tvec)

# according to documentation you should use "fpca.bspline". This is wrong just use "bspline".
# dont use standart simpson. It is remmomended for non-equidistant grids to use riemann or trapezodial
formula1 = response ~ lf(slopet, argvals = tvec, presmooth = "bspline", presmooth.opts = list(nbasis = 4), integration = "riemann")+ lf(aspect_nst)

fit = fgam(formula = formula1, family = binomial())

fit = pfr(formula = formula1, family = binomial(), )
fit$pred.formula

summary(fit)


data(gasoline)

# Create the requisite functional data objects
bbasis = create.bspline.basis(c(900, 1700), 40)
wavelengths = 2*450:850
nir <- t(gasoline$NIR)
gas.fd = smooth.basisPar(wavelengths, nir, bbasis)$fd

a = gasoline
# Method 1: Call fpcr with fdobj argument
gasmod1 = fpcr(gasoline$octane, fdobj = gas.fd, ncomp = 30)
gasmod1
summary(gasmod1)
plot(gasmod1, xlab="Wavelength")
## Not run:
# Method 2: Call fpcr with explicit signal matrix
gasmod2 = fpcr(gasoline$octane, xfuncs = gasoline$NIR, ncomp = 30)
plot(gasmod2, xlab="Wavelength")
# Method 3: Call fpcr with explicit signal, basis, and penalty matrices
gasmod3 = fpcr(gasoline$octane, xfuncs = gasoline$NIR,
               basismat = eval.basis(wavelengths, bbasis),
               penmat = getbasispenalty(bbasis), ncomp = 30)

plot(gasmod3, xlab="Wavelength")
# Check that all 3 calls yield essentially identical estimates
all.equal(gasmod1$fhat, gasmod2$fhat, gasmod3$fhat)
# But note that, in general, you'd have to specify argvals in Method 1
# to get the same coefficient function values as with Methods 2 & 3.

## End(Not run)

### 2D functional predictor example ###

n = 200; d = 70

# Create true coefficient function
ftrue = matrix(0,d,d)
ftrue[40:46,34:38] = 1

# Generate random functional predictors, and scalar responses
ii = array(rnorm(n*d^2), dim=c(n,d,d))
iimat = ii; dim(iimat) = c(n,d^2)
yy = iimat %*% as.vector(ftrue) + rnorm(n, sd=.3)

mm = fpcr(yy, ii, ncomp=40)

image(ftrue)
contour(mm$fhat, add=TRUE)

## Not run:
### Cross-validation ###
cv.gas = fpcr(gasoline$octane, xfuncs = gasoline$NIR,
              nbasis=seq(20,40,5), ncomp = seq(10,20,5), store.cv = TRUE)
image(seq(20,40,5), seq(10,20,5), cv.gas$cv.table, xlab="Basis size",
      ylab="Number of PCs", xaxp=c(20,40,4), yaxp=c(10,20,2))

