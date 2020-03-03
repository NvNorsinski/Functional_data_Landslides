rm(list = ls(all = TRUE))
library(fda.usc)
library(raster)
library(ggplot2)
# read data
response = readRDS(file = "Daten/Paldau/Samples/response.rds")

slope = readRDS(file = "Daten/Paldau/Samples/slope.rds")
dgm = readRDS(file = "Daten/Paldau/Samples/dgm.rds")
aspect_ns = readRDS(file = "Daten/Paldau/Samples/aspect_ns.rds")
aspect_ow = readRDS(file = "Daten/Paldau/Samples/aspect_ow.rds")

genCurvature = readRDS(file = "Daten/Paldau/Samples/genCurvature.rds")
catchmant_area = readRDS(file = "Daten/Paldau/Samples/catchmantArea.rds")

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")


model = readRDS("Daten/Paldau/Outputs/model.rds")
modelglm = readRDS("Daten/Paldau/Outputs/glm_model.rds")
imgfunc = raster("Daten/Paldau/Outputs/prob_map_fregre_glm_cor.tif")
imgn = raster("Daten/Paldau/Outputs/glm.tif")
#-------------------------------------------------------------------------------


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



i = 5
j = 4


# create basis object
number_knots = i

knotvec = lseq(from = min, to = num_scenes, length.out = number_knots)


# cubic bsplines -> 4
polynomial_order = j

basis.x = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)
basis.b = create.bspline.basis(rangeval, norder = polynomial_order, breaks = knotvec)


dgm_basis = smooth.basis(tvec, dgm, basis.x)
asp_ns_basis = smooth.basis(tvec, aspect_ns, basis.x)
asp_ow_basis = smooth.basis(tvec, aspect_ow, basis.x)
slope_basis = smooth.basis(tvec, slope, basis.x)

genCurv_basis = smooth.basis(tvec, genCurvature, basis.x)
catch_basis = smooth.basis(tvec, catchmant_area, basis.x)

twi_basis = smooth.basis(tvec, twi, basis.x)


#plot(basis.x)

# prepare data

dgmfd = fdata(dgm_basis$fd)

slopefd = fdata(slope_basis$fd)

aspect_nsfd = fdata(asp_ns_basis$fd)
aspect_owfd = fdata(asp_ow_basis$fd)
genCurvfd = fdata(genCurv_basis$fd)

catchfd = fdata(catch_basis$fd)

twifd = fdata(twi_basis$fd)

list_fd = list(slope_basis, dgm_basis, asp_ns_basis, asp_ow_basis, genCurv_basis, catch_basis, twi_basis)



# all variables + non functional variable---------------------------------------
formula1 = response ~ slope + aspect_ns + aspect_ow + genCurvature +
  catchmant_area + twi


#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = as.data.frame(response)

df = cbind(df)

ldata1 = list(df = df, twi = twifd, slope = slopefd,
              aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
              genCurvature = genCurvfd,
              catchmant_area = catchfd)


pred = predict(model, newx = ldata1, type = "response",
               se.fit = TRUE, level = 0.95)
pred

model$fitted.values

predm = as.vector(model$fitted.values)
predm
pred = as.vector(pred$fit)
pred

plot(predm, pred)

model$linear.predictors

boxplot(pred$fit)
pred = as.data.frame(pred)
pr_ft = pred$fit
hist(pr_ft)




#hist(img)

ggplot(pred) +
  geom_density(aes(x = fit)) +
  geom_rug(aes(x = fit, y = 0), position = position_jitter(height = 0))+
  xlim(0, 1)+
  ylim(0, 2)+
  xlab("Predict") +
  # labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2))+
  theme_minimal()

imgdatfunc = as.data.frame(imgfunc)
imgdatn = as.data.frame(imgn)

imgdatn$values

names(imgdatfunc) = "values"
names(imgdatn) = "values"

predfglm = readRDS("Daten/Paldau/predfglm.rds")
predfglm
predglm = readRDS("Daten/Paldau/Pred_glm.rds")
par(mar=c(5.1, 5.1, 5.1, 8.1), xpd=TRUE)

png("density_pred_legend.png", type="cairo", width = 800, height = 400, units = "px" )


plot(density(imgdatfunc$values, bw = 0.005, cut = c(0,1)),col = "red",lwd = 2, main = "",
     zero.line = FALSE, xlab = "Predictions", ylim = c(0,25),yaxs="i", xaxs="i", cex.axis = 1.5, cex.lab = 1.5)
lines(density(model$fitted.values, bw = 0.005), col = "blue", lwd = 2)
lines(density(modelglm$fitted.values, bw = 0.005), col = "green", lwd = 2)

#legend("topright", inset=c(-0.4,0), legend=c("Map Prediction","Training Prediction FGLM", "Training Prediction GLM"),
#       col=c("red", "blue", "green" ), lty=1, cex=1)


dev.off()

aa = as.data.frame(model$fitted.values)
bb = as.data.frame(modelglm$fitted.values)
plot(model$fitted.values)
plot(modelglm$fitted.values)
#plot(density(imgr$values, bw = 0.005), main = "Image Data", xlab = "Predictions")
#rug(jitter(pred$fit))


ggplot(imgr) +
  geom_density(aes(x = values)) +
  geom_rug(aes(x = values, y = 0), position = position_jitter(height = 0))+
  xlim(0, 1)+
  ylim(0, 2)+
  xlab("Predict") +
  # labs(title="Slope")+
  #theme(legend.position="bottom")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2))+
  theme_minimal()
