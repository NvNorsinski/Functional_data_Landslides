rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)


# read data
slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect = readRDS(file = "Daten/Paldau/Samples/aspect.rds")
response = readRDS(file = "Daten/Paldau/Samples/response.rds")



num_observations = nrow(slope)
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

plot(dgm_basis)


beta_basis = create.bspline.basis(rangeval, norder = polynomial_order)
dgmfd = fdata(t(dgm))
slopefd = fdata(t(slope))


res.basis0 = fregre.basis(dgmfd, y = response , basis.x = dgm_basis, basis.b = beta_basis)

res.basis0
summary(res.basis0)
yfit = ifelse(res.basis0$fitted.values < 0.5, 0, 1)
table(response, yfit)
plot(res.basis0$beta.est)

#
ldata = list(response = response, dgm = dgmfd, slope = slopefd)
formula = response ~ dgm + slope

res.basis0 = fregre.lm(formula, data = ldata, basis.x = dgm_basis, basis.b = beta_basis)

res.basis0
