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

respons = as.factor(response)
# it is necessary to repeat the response vector for each included variable
number_variables = 6

# preparation of data-----------------------------------------------------------
# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

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

# cubic bsplines -> 4
polynomial_order = 4

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.x
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
slopefd = fdata((slope), rangeval = rangeval, argvals = tvec)

plot(slopefd)
plot(slope_basis)

aspect_nsfd = fdata(t(aspect_ns), rangeval = rangeval, argvals = tvec)
aspect_owfd = fdata(t(aspect_ow), rangeval = rangeval, argvals = tvec)
genCurvfd = fdata(t(genCurvature), rangeval = rangeval, argvals = tvec)

catchfd = fdata(t(catchmant_area), rangeval = rangeval, argvals = tvec)
tpifd = fdata(t(tpi), rangeval = rangeval, argvals = tvec)
#twifd = fdata(t(twi))

# replicate response variable for each variable included in the model
response_rep = rep(response, number_variables)

#-------------------------------------------------------------------------------
formula1 = response ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
  catchmant_area + geology

formula2 = response ~ slope + aspect_ns + aspect_ow + tpi +catchmant_area+ geology

formula3 = response ~ slope




#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = as.data.frame(response)
df = cbind(df, geology)

ldata1 = list(df = df, slope = slopefd, tpi = tpifd,
             aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd, genCurvature = genCurvfd,
             catchmant_area = catchfd)

ldata2 = list(df = df, slope = slopefd, aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
              tpi = tpifd, catchmant_area = catchfd)

ldata3 = list(df = df, slope = slopefd)




res.basis1 = fregre.glm(formula1, data = ldata1, basis.x = basis.x,
                        basis.b = basis.b, family = binomial(link = "logit"))
res.basis1
res.basis1$effects
summary(res.basis1)

res.basis1

# influential value
plot(res.basis1, which = 4, id.n = 3)

# multicolinearität
# 5 - 10 bedeutet ein großes maaß an kolinearität
col = car::vif(res.basis1)
col

# test in other cuttoff values then 0.5
# does not improve results

# for(p in seq(0.05, 0.95,0.05)){
#   yfit = ifelse(res.basis1$fitted.values < p, 0, 1)
#
#   text = table(response, yfit)
#   print(text)
#   corr = sum(diag(text)) / sum(text)
#   print(corr)
#   print(p)
# }

yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)

text = table(response, yfit)
text



library(ROCR)

response = as.factor(response)
obs = response
pred = res.basis1$fitted.values

predobj <- prediction(pred, obs)
auroc <- performance(predobj, measure = "auc")@y.values[[1]]
auroc





yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)

text = table(response, yfit)
text


plot(res.basis1$mean$slope)
res.basis1$qr$qr

summary(res.basis1)

par(mfrow=c(2,2))
plot(res.basis1)
par(mfrow=c(1,1))

# plot confidence intervall
plot(res.basis1 ,which=1)
plot(predict(res.basis1),residuals(res.basis1))
abline(h=0,lty=2,col="grey")
lines(lowess(predict(res.basis1),residuals(res.basis1)),col="red",lwd=1)
rl=lm(residuals(res.basis1)~bs(predict(res.basis1), 6))
y=predict(rl, se=TRUE)
segments(predict(res.basis1), y$fit+2*y$se.fit, predict(res.basis1), y$fit-2*y$se.fit,col="green")


yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)
table(response, yfit)

plot(res.basis1$beta.l$slope)


conf = confint(res.basis1, level = 0.95)
conf

#res.boot1=fregre.bootstrap(res.basis1, nb=100,wild=FALSE,kmax.fix=TRUE, draw = TRUE)


linkinv <- family(res.basis1)$linkinv
pred = predict.fregre.glm(res.basis1, newx = ldata1, se.fit = TRUE, type = "link")
plot(pred$pred)

pred$pred<-linkinv(pred$fit)
pred$LC<-linkinv(pred$fit-1.96*pred$se.fit)
pred$UC<-linkinv(pred$fit+1.96*pred$se.fit)


dd = data.frame(pred, pred$UC, pred$LC)

plot(dd)

plot(res.basis1$beta.l$slope)
plot(dd$fit, type = "l" )



p = geom_ribbon(data = dd,
                  aes(ymin = right_lwr, ymax = right_upr),
                  alpha = 0.1)


p
#newldata=list(slope,slopefd)


#pred = predict.fregre.glm(res.basis1, se.fit=TRUE, level = 0.95, newx = newldata)
pframe <- data.frame(site_name=unique(ldata1$slope))

# same thing
res.gsam = classif.glm(formula1, data=ldata1)
summary(res.gsam)

par(mfrow=c(2,2))
plot(res.basis1)
par(mfrow=c(1,1))



res.basis1$coefficients

res.basis1$family


res.basis1$beta.l$dgm$coefs


plot(res.basis1$beta.l$slope, main = "slope", xlab ="number of scene" , ylab = "beta")
plot(res.basis1$beta.l$aspect, main = "aspect", xlab ="number of scene" , ylab = "beta")
plot(res.basis1$beta.l$tpi, main = "tpi",  xlab ="size of moving window" , ylab = "beta")
plot(res.basis1$beta.l$catchmant_area, main = "Catchmant area",  xlab ="size of moving window" , ylab = "beta")



res.DD<-classif.DD(response, ldata3, classif="lda", depth="mode")



