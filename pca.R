rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)


#unlink("Daten/output/out.txt")
#unlink("outfile.txt")




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

#out_pca = "out_pca.txt"

# it is necessary to repeat the response vector for each included variable
number_variables = 7

leng = length(slope[1,])

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



dat = cbind(slope, tpi, catchmant_area, aspect_ns, aspect_ow, twi, genCurvature)

fdafd = smooth.basis(tvec, dat, basis.x)
fdafd = fdata(fdafd$fd)

# rename colums in functional dataframe
nmbr = seq(1:leng)
sl_name = rep("slope", leng)
sl_name = paste0(sl_name," ",nmbr)

tpi_name = rep("tpi", leng)
tpi_name = paste0(tpi_name, " ",nmbr)

ca_name = rep("catchmant_area", leng)
ca_name = paste0(ca_name," ",nmbr)

an_name = rep("aspect_ns", leng)
an_name = paste0(an_name," ",nmbr)

ao_name = rep("aspect_ow", leng)
ao_name = paste0(ao_name," ",nmbr)

twi_name = rep("twi", leng)
twi_name = paste0(twi_name," ",nmbr)

gc_name = rep("genCurvature", leng)
gc_name = paste0(gc_name," ",nmbr)


names = c(sl_name, tpi_name, ca_name, an_name, ao_name, twi_name, gc_name)

rownames(fdafd$data) = names



# pca---------------------------------------------------------------------------

# pca regression
res = fregre.pc(fdafd, y = response_rep, basis.x = basis.x, P = c(1,0,0), lambda = 0)
res

plot(res$beta.est)
# pca
res = fdata2pc(fdafd,  ncomp = 3,norm = TRUE,lambda=0, P=c(1,0,0))
res



# summary(res, y = response_rep)




cc = fdafd$coefs


# criteria
lambda =  c(0, 10**seq(-3, 10, by = 0.25))



# penalizaton off
#lambda = 0
#res2 = fregre.pc.cv(fdafd, y = response_rep, lambda = lambda, P=c(1,0,0), basis.x = basis.x, kmax = 4, criteria = "SIC")
#kmax = 40

# maximum of kmax number of splines

kmax = seq(1:6)
res2 = fregre.pc.cv(fdafd, y = response_rep, lambda = 0, P=c(1,0,0), basis.x = basis.x, kmax = 6, criteria = "SIC")
res2
res2$PC.order
res2$MSC.order
res2$pc.opt
res2$lambda.opt
res2$fregre.pc$lm
cat(paste0(res2), file=out_pca, append = TRUE)

########

