# Author: Nils von Norsinski
# genrerate output of models with different parameters of knots and polynomial
# order

rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)
library(ROCR)

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

list_dat = list(slope, dgm, aspect_ns, aspect_ow, genCurvature, catchmant_area, tpi, twi)
list_nam = c("slope", "dgm", "aspect_ns", "aspect_ow", "genCurvature", "catchmant_area", "tpi", "twi")
leng_list = length(list_dat)

out_pca = "out_pca.txt"


respons = as.factor(response)

# replicate response variable for each variable included in the model



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

#number_knots = seq(3:8)

# to high polynomial order leads to artefacts. keep order at 6 or below
# keep number of knots at 10 or below

# number of knots = i
# polynomial order = j
for (i in 3:3) {
  for (j in 3:3){


    # create basis object
    number_knots = i
    print(paste0("i: ", i))
    print(paste0("j: ", j))

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

    list_fd = list(slope_basis, dgm_basis, asp_ns_basis, asp_ow_basis,
                   genCurv_basis, catch_basis, tpi_basis, twi_basis)



    # all variables + non functional variable-----------------------------------
    formula1 = response ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
      catchmant_area + twi + geology


    #basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
    df = as.data.frame(response)

    df = cbind(df, geology)

    ldata1 = list(df = df, twi = twifd, slope = slopefd, tpi = tpifd,
                 aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
                 genCurvature = genCurvfd,
                 catchmant_area = catchfd)


    res.basis1 = fregre.glm(formula1, data = ldata1, basis.x = basis.x,
                            basis.b = basis.b, family = binomial(link = "logit"))

    res.basis1


    cat(paste0("-------------------------------------------------Number of knots ",
               i, "Polynomial Order ", j), file="outfile.txt", sep="\n",
        append = TRUE)



    text = capture.output(summary(res.basis1), split = FALSE)
    cat(text, file="outfile.txt", sep="\n", append = TRUE)


    yfit = ifelse(res.basis1$fitted.values < 0.5, 0, 1)

    text = table(response, yfit)

    # calc auroc
    response = as.factor(response)
    obs = response
    pred = res.basis1$fitted.values

    predobj = prediction(pred, obs)
    auroc = performance(predobj, measure = "auc")@y.values[[1]]


    #
    cat("RMSE--------------------", file="outfile.txt", sep="\n", append = TRUE)
    for (k in 1:leng_list){

      RMSE = sqrt(mean((eval.fd(tvec, list_fd[[k]]$fd) - list_dat[[k]])**2))

      RMSE

      cat(paste0(list_nam[k], ": ", RMSE), file="outfile.txt", sep="\n",
          append = TRUE)

    }

    # write table to file
    cat("\t", "yfit", file="outfile.txt", sep="\n", append = TRUE)
    cat("response", "\t", 0, "\t", 1, "\n", file="outfile.txt", append = TRUE)
    cat("0", "\t", text[1], "\t", text[2], "\n", file="outfile.txt",
        append = TRUE)
    cat("1", "\t", text[3], "\t", text[4], "\n", file="outfile.txt",
        append = TRUE)

    # write auroc
    cat("", file="outfile.txt", sep="\n", append = TRUE)

    cat("AUROC: ",auroc, file="outfile.txt", sep="\n", append = TRUE)



    name = paste0("Daten/images/summary", i, j, ".png")
    png(filename=name, width = 900, height = 900)

    par(mfrow=c(2,2), oma = c(0, 0, 2, 0))

    plot(res.basis1)
    mtext(paste0("Nr. knots: ", i, " ", "Polynomial Order: ", j),
          outer = TRUE, cex = 1, line = 1)


    par(mfrow=c(1,1))
    dev.off()


    res.basis1$coefficients


    res.basis1$beta.l$slope$coefs

    name = paste0("Daten/images/regress",i,j,".png")
    png(filename=name, width = 900, height = 900)
    par(mfrow=c(2,4))
    plot(res.basis1$beta.l$slope,
         main = paste0("slope ","n. knots",i,"p. order", j),
         xlab = "size moving window [m]", ylab = "beta [Grad]" , yaxt="n",
         xaxt="n", cex.lab = 2)

    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$twi, main = "twi", xlab = "size moving window [m]",
         ylab = "beta", yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$tpi, main = "tpi", xlab = "size moving window [m]",
         ylab = "beta [m]", yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$catchmant_area, main = "Catchmant area",
         xlab = "size moving window [m]",
         ylab = "beta (log)", yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$aspect_ns, main = "aspect ns",
         xlab = "size moving window [m]", ylab = "beta [rad]",
         yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$aspect_ow, main = "aspect ew",
         xlab = "size moving window [m]", ylab = "beta [rad]",
         yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    plot(res.basis1$beta.l$genCurvature, main = "general Curvature",
         xlab = "size moving window [m]", ylab = "beta [rad]",
         yaxt="n", xaxt="n", cex.lab = 2)
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)

    par(mfrow=c(1,1))

    dev.off()
  }
}

