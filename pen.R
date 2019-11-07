rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)

unlink("Daten/output/out.txt")
unlink("outfile.txt")




# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
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
twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")

out_pca = "out_pca.txt"

# it is necessary to repeat the response vector for each included variable
number_variables = 7

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)


# preparation of data-----------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

num_scenes = 50
min = 1

num_points = ncol(slope)

tvec = seq(from = 1, to = 15, by = 1)
tvec2 = seq(from = 20, to = 50, by = 5)
tvec = append(tvec, tvec2)

rangeval = c(min, num_scenes)



# create basis object
number_knots = 5


knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)


# cubic bsplines -> 4
polynomial_order = 4

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)


dgm_basis = smooth.basis(tvec, dgm, basis.x)
asp_ns_basis = smooth.basis(tvec, aspect_ns, basis.x)
asp_ow_basis = smooth.basis(tvec, aspect_ow, basis.x)
slope_basis = smooth.basis(tvec, slope, basis.x)

genCurv_basis = smooth.basis(tvec, genCurvature, basis.x)
catch_basis = smooth.basis(tvec, catchmant_area, basis.x)
tpi_basis = smooth.basis(tvec, tpi, basis.x)
twi_basis = smooth.basis(tvec, twi, basis.x)


#plot(basis.x)

# prepare data

dgmfd = fdata(dgm_basis$fd)

slopefd = fdata(slope_basis$fd)

aspect_nsfd = fdata(asp_ns_basis$fd)
aspect_owfd = fdata(asp_ow_basis$fd)
genCurvfd = fdata(genCurv_basis$fd)

catchfd = fdata(catch_basis$fd)
tpifd = fdata(tpi_basis$fd)
twifd = fdata(twi_basis$fd)


# ridge regression--------------------------------------------------------------
formula1 = response ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
  catchmant_area + twi + geology


#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = as.data.frame(response)

df = cbind(df, geology)

ldata1 = list(df = df, twi = twifd, slope = slopefd, tpi = tpifd,
              aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
              genCurvature = genCurvfd,
              catchmant_area = catchfd)



# CV = TRUE -> error
res.basis = fregre.glm(formula1, data = ldata1, basis.x = basis.x, family = "binomial")
res.basis
summary(res.basis)
plot(res.basis)

yfit = ifelse(res.basis$fitted.values < 0.5, 0, 1)
table(response, yfit)







