
# Author Nils von Norsinski
rm(list = ls(all = TRUE))
library(fda)
library(fda.usc)
library(ggplot2)
library(rasterVis)
library(rgdal)

library(sf)
library(raster)
library(foreach)
library(doParallel)
#cl <- makeCluster(2)
#registerDoParallel(cl, cores = 2)

library(ggsn)

# read data
# read data

img = brick("Daten/Paldau/Parameters/hillshade_reduced.tif")

landslides_shp <-st_read("Daten/Paldau/Landslides/allLandslides.shp")
#plot(img)
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

twi = readRDS(file = "Daten/Paldau/Samples/twi.rds")

poi = readRDS(file = "Daten/Paldau/Samples/geometry.rds")

response = as.factor(response)


raster1.pts <- rasterToPoints(img)
raster1.df <- data.frame(raster1.pts)
#-------------------------------------------------------------------------------
extend = bbox(img)

xmin = min(extend[1,1])
xmax = max(extend[1,2])

ymin = min(extend[2,1])
ymax = max(extend[2,2])



#changeable
resolution = 64

x.seq = seq(xmin, xmax, by = resolution)
y.seq = seq(ymin, ymax, by = resolution)

t.grid = expand.grid(x.seq, y.seq)
names(t.grid) = c("x", "y")


t.grid_sf = st_as_sf(t.grid, coords = c("x", "y"))

saveRDS(t.grid_sf, paste0("Daten/Paldau/Samples/t.grid.rds"))

#poi = t.grid

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

i = 6
j = 10



knotvec = lseq(from = min, to = num_scenes, length.out = i)
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

list_fd = list(slope_basis, dgm_basis, asp_ns_basis, asp_ow_basis, genCurv_basis, catch_basis, tpi_basis, twi_basis)



# all variables + non functional variable---------------------------------------
formula1 = response ~ slope + aspect_ns + aspect_ow + tpi + genCurvature +
  catchmant_area + twi


#basis.list = list(dgm.x = dgm_basis, slope.x = slope_basis)
df = as.data.frame(response)

df = cbind(df)

ldata1 = list(df = df, twi = twifd, slope = slopefd, tpi = tpifd,
              aspect_ns = aspect_nsfd, aspect_ow = aspect_owfd,
              genCurvature = genCurvfd,
              catchmant_area = catchfd)


res.basis1 = fregre.glm(formula1, data = ldata1, basis.x = basis.x,
                        basis.b = basis.b, family = binomial(link = "logit"))

# svae the model
saveRDS(res.basis1,"Daten/Paldau/Samples/model.rds" )

prob = as.data.frame(res.basis1$fitted.values)

prob = cbind(prob, poi)

names(prob)[1] <- "proba"
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------



prob$proba = round(prob$proba, 3)

xy <- prob[,c(2,3)]

crs = crs(img)

spdf <- SpatialPointsDataFrame(coords = xy, data = as.data.frame(prob$proba),
                               proj4string = crs)


test_spdf <- as(img, "SpatialPixelsDataFrame")

test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")


df <- st_as_sf(x = prob,
               coords = c("x", "y"),
               crs = crs)


# plot hillshade  and sample points

gplot(img, maxpixels = 5e5) +
  geom_tile(aes(fill = value), show.legend = FALSE) +
  #facet_wrap(~ variable) +
  scale_fill_gradient(low = 'black', high = 'white') +
  coord_equal()+
  geom_point(data = prob, aes(x = x, y = y, colour = proba))+
  scale_color_gradient(low = 'green', high = 'red', limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1), name="Probability [%]")+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.title = element_text(hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 20, b = 20, l = 0)))+

  north(df, symbol = 12,location = "topright" ) +
  scalebar(df, dist = 1, dist_unit = "km",
           transform = FALSE, st.size = 3, height = 0.01,
           location = "bottomright"
           )+
  xlab("Longitude") + ylab("Latitude")+
  labs(title = "Switgraphics",
       caption = "ckdmc \n dsjnds")






# plot hillshade and sample points and landslide shape file
img1 = ggplot(data=raster1.df, maxpixels= 5e1)+

   geom_tile(aes(x, y, fill=raster1.df[[3]]), show.legend = FALSE)+
   scale_fill_gradient(low = 'black', high = 'white')+
   coord_equal()+
  geom_sf(data = landslides_shp)+
   geom_point(data = prob, aes(x = x, y = y, colour = proba))+
   scale_color_gradient(low = 'green', high = 'red', limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1), name="Probability [%]")+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 20, b = 20, l = 0)))+
  north(df, symbol = 12) +
  scalebar(df, dist = 1, dist_unit = "km",
           transform = FALSE, st.size = 3, height = 0.01)+
  xlab("Longitude") + ylab("Latitude")

#png("img1.png")
print(img1)
#dev.off()









