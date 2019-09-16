rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)


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
# something is wrong with this dataset
#twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")


# it is necessary to repeat the response vector for each included variable
number_variables = 5

# preparation of data-----------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

# create basis object
number_knots = 4
num_scenes = 50
min = 1

knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)
num_points = ncol(slope)

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by= 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)

# cubic bsplines -> 4
polynomial_order = 4
basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
plot(basis.x)

#dgm_basis = smooth.basis(tvec, dgm, basis.x)
asp_ns_basis = smooth.basis(tvec, aspect_ns, basis.x)
asp_ow_basis = smooth.basis(tvec, aspect_ow, basis.x)
slope_basis = smooth.basis(tvec, slope, basis.x)

genCurv_basis = smooth.basis(tvec, genCurvature, basis.x)
catch_basis = smooth.basis(tvec, catchmant_area, basis.x)
tpi_basis = smooth.basis(tvec, tpi, basis.x)
#twi_basis = smooth.basis(tvec, twi, basis.x)


#plot(slope_basis)

# prepare data
# transposing dataframes is necessary
#dgmfd = fdata(t(dgm))
slopefd = fdata(t(slope))

aspect_nsfd = fdata(t(aspect_ns))
aspect_owfd = fdata(t(aspect_ow))
genCurvfd = fdata(t(genCurvature))

catchfd = fdata(t(catchmant_area))
tpifd = fdata(t(tpi))
#twifd = fdata(t(twi))

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)

#-------------------------------------------------------------------------------
formula1 = response ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
  catchmant_area + geology

formula2 = response ~ slope + aspect_ns + aspect_ow + tpi +catchmant_area+ geology

formula3 = response ~ slope + geology




#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = as.data.frame(response)
df = cbind(df, geology)

ldata1 = list(df = df, slope = slopefd, tpi = tpifd,
             aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd, genCurvature = genCurvfd,
             catchmant_area = catchfd)

ldata2 = list(df = df, slope = slopefd, aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
              tpi = tpifd, catchmant_area = catchfd)

ldata3 = list(df = df, slope = slopefd)



# TODO including catchmant area results in singularity error
res.basis1 = fregre.glm(formula1, data = ldata1, basis.x = basis.x,
                        basis.b = basis.b, family = binomial(link = "logit"))
res.basis1

summary(res.basis1)

yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)
table(response, yfit)

par(mfrow=c(2,2))
plot(res.basis1)
par(mfrow=c(1,1))



res.basis1$coefficients

res.basis1$family


res.basis1$beta.l$dgm$coefs


plot(res.basis1$beta.l$slope, main = "slope")
plot(res.basis1$beta.l$aspect, main = "aspect")
plot(res.basis1$beta.l$tpi, main = "tpi")
plot(res.basis1$beta.l$catchmant_area, main = "Catchmant area")
