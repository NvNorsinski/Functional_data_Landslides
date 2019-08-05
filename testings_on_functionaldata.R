rm(list = ls(all = TRUE))
library(fda)
library(sf)

# generate logarithmic sequence
lseq = function(from, to, length.out) {
  return(exp(seq(log(from), log(to), length.out = length.out)))
}

# read in data
slope_dat = readRDS(file = "Daten/Paldau/Parameters/filtered_slope_pointVal.rds")
dgm_dat = readRDS(file = "Daten/Paldau/Parameters/filtered_dgm_pointVal.rds")


# remove spatial data
dgm_dat1 = sf::st_drop_geometry(dgm_dat)
# and remove id colum
dgm_dat1 = dgm_dat1[c(-1)]
dgm_dat1 = t(as.matrix(dgm_dat1))

# same thing like above
slope_dat1 = sf::st_drop_geometry(slope_dat)
slope_dat1 = slope_dat1[c(-1)]
slope_dat1 = t(as.matrix(slope_dat1))


file = dgm_dat1
# generate vector with length of measured spatial points
leng = length(file[,1])
length_vec = seq(1:leng)
length_vec = as.numeric(length_vec)


# highest and lowest value must be in first image,
# because of averging out with greater filter size in further images

max = leng
min = 1
# position of knots
number_knots = 5
knotvec = lseq(from = min, to = max, length.out = number_knots)


rangeval = c(min, max)

basisobj1 = create.bspline.basis(rangeval, norder = 5, breaks = knotvec)
plot(basisobj1)
summary(basisobj1)

basisobj2 = create.bspline.basis(rangeval, norder = 15)
plot(basisobj2)
summary(basisobj2)

basisobj3 = create.bspline.basis(rangeval, nbasis = 7)
plot(basisobj3)
summary(basisobj3)


#basismatrix = eval.basis(knotvec, basisobj3)
#basismatrix

# derivative
#Dbasismatrix = eval.basis(knotvec, basisobj1 , 1)
#Dbasismatrix



fdnames = list("Grade of smoothnes of image","", "Height (m)")

tempfd = smooth.basis(length_vec, dgm_dat1, basisobj3)$fd
tempfd$fdnames = fdnames

plot(tempfd)


fdnames1 = list("Grade of smoothnes of image","", "Slope")

tempfd1 = smooth.basis(length_vec, slope_dat1, basisobj2)$fd
tempfd1$fdnames = fdnames1

plot(tempfd1)

#------
lambda = 0.01
slopefd = fdPar(fdobj = basisobj1, 2, lambda = lambda)
slopefd
heightmat
