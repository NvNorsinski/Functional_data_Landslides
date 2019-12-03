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

out_pca = "out_pca_lambda.txt"

# it is necessary to repeat the response vector for each included variable
number_variables = 7

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)
response_rep <-as.factor(response_rep)

leng = length(slope[1,])

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
number_knots = 6


knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)


# cubic bsplines -> 4
polynomial_order = 4

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)

# smoothed data
dgm_sm = smooth.basis(tvec, dgm, basis.x)
asp_ns_sm = smooth.basis(tvec, aspect_ns, basis.x)
asp_ew_sm = smooth.basis(tvec, aspect_ow, basis.x)
slope_sm = smooth.basis(tvec, slope, basis.x)

genCurv_sm = smooth.basis(tvec, genCurvature, basis.x)
catch_sm = smooth.basis(tvec, catchmant_area, basis.x)
tpi_sm = smooth.basis(tvec, tpi, basis.x)
twi_sm = smooth.basis(tvec, twi, basis.x)




# unsmoothed data, but in fdata format
dgmfd = fdata(dgm_sm$fd, argvals = tvec)
slopefd = fdata(slope_sm$fd, argvals = tvec)
aspect_nsfd = fdata(asp_ns_sm$fd, argvals = tvec)
aspect_owfd = fdata(asp_ew_sm$fd, argvals = tvec)

genCurvfd = fdata(genCurv_sm$fd, argvals = tvec)
catchfd = fdata(catch_sm$fd, argvals = tvec)
tpifd = fdata(tpi_sm$fd, argvals = tvec)
twifd = fdata(twi_sm$fd, argvals = tvec)

# singularity error starts with a lambda value of 3.162278e+09
# number of basis needs to be nb = seq(4, 6, by = 1)
lambda =  c(0, 10**seq(1, 9.25, by = 0.25))


nb <- seq(4, 6, by = 1)
# of lambda = 0 this is runnable
nb2 <- seq(4, 13, by = 1)

obj = list(dgmfd, slopefd, aspect_nsfd, aspect_owfd, genCurvfd, catchfd, tpifd, twifd)


out0 <- min.basis(slopefd, lambda = lambda, numbasis = nb, type.basis = "bspline", type.CV = GCV.S)
out1 <- min.np(slopefd, type.CV = GCV.S)
out0


# error variance

np<-ncol(slopefd)
z=qnorm(0.025/np)

fdata.est = out0$fdata.est
var.e<-Var.e(tpifd, out1$S.opt)
var.y<-Var.y(tpifd, out1$S.opt)
var.y2<-Var.y(tpifd, out1$S.opt,var.e)


upper.var.e<-fdata.est[1,]-z*sqrt(diag(var.e))
lower.var.e<-fdata.est[1,]+z*sqrt(diag(var.e))


plot(slopefd[1,],lwd=1,
     ylim=c(min(lower.var.e$data),max(upper.var.e$data)),xlab="size")

lines(fdata.est[1,],col=gray(.1),lwd=1)
lines(fdata.est[1,]+z*sqrt(diag(var.y)),col=gray(0.7),lwd=2)
lines(fdata.est[1,]-z*sqrt(diag(var.y)),col=gray(0.7),lwd=2)
lines(upper.var.e,col=gray(.3),lwd=2,lty=2)
lines(lower.var.e,col=gray(.3),lwd=2,lty=2)
legend("bottom",legend=c("Var.y","Var.error"),
       col = c(gray(0.7),gray(0.3)),lty=c(1,2))

summary(out0)
plot(out0$gcv)

out0$gcv.opt
# optimal number of basis seems to be always the highest number
out0$numbasis.opt

out0$lambda.opt


# plot error of splining in comparrison to orriginal

#plotfit.fd(dgm, tvec,  fdata2fd(dgmfd, type.basis = "bspline"))

RMSE = sqrt(mean((eval.fd(tvec, dgm_sm$fd) - dgm)**2))
RMSE


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

res.gsam<-classif.glm(formula1, data = ldata1)

summary(res.gsam)







