# generate filtered images with different size of moving window
rm(list = ls(all = TRUE))
library(RQGIS)



# set path to qgis installation. Version 2.x is needed
set_env("C:/Program Files/QGIS 2.18")

# define input image
path_to_slope_image = "Daten/Paldau/Parameters/slope_paldau.tif"
path_to_dgm = "Daten/Paldau/Parameters/filtered_images_dgm"

# define output image path
path_to_out = "Daten/Paldau/Parameters/"


# define output image name
slope = "Daten/Paldau/Parameters/filtered_images_slope/"
aspect = "Daten/Paldau/Parameters/Filtered_images_aspect/"
general_curvature = "Daten/Paldau/Parameters/Filtered_images_generalCurvature/"
plan_curvature = "Daten/Paldau/Parameters/Filtered_images_planCurvature/"

catchmant_area = "Daten/Paldau/Parameters/Filtered_images_catchmant_area/"
twi_image = "Daten/Paldau/Parameters/Filtered_images_twi/"

#
#for (i in fs){
#  img = raster(i)
#  terrain(img, opt=c("slope", "aspect", "TPI", "TRI","roughness"), unit = "degrees", neighbors = 8)
#}

get_usage(alg = "saga:slopeaspectcurvature")

params = get_args_man(alg = "saga:slopeaspectcurvature")
params

fs = list.files(path = path_to_dgm, pattern = "tif$", full.names = TRUE)
fs

count = 0
for (i in fs){


  params$ELEVATION = i
  params$SLOPE = paste0(slope ,"slope_", 100 + count)
  params$ASPECT = paste0(aspect,"aspect_", 100 + count)
  params$C_GENE = paste0(general_curvature,"generalCurvature_", 100 + count)
  params$C_PLAN = paste0(plan_curvature,"planCurvature_", 100 + count)
  params$METHOD = 6
  params$UNIT_SLOPE = 1
  params$UNIT_ASPECT = 1


  out = run_qgis(alg = "saga:slopeaspectcurvature",
                 params = params)
  count = count + 1
}




# calculate catchment area
get_usage(alg = "saga:flowaccumulationrecursive")

params = get_args_man(alg = "saga:flowaccumulationrecursive")
params

count = 18
for (i in fs){

  params$ELEVATION = i
  params$STEP = 1
  params$METHOD = 0
  params$CONVERGENCE = 1.1
  params$FLOW = paste0(catchmant_area,"catchmant_area_", 100 + count)

  out = run_qgis(alg = "saga:flowaccumulationrecursive",
                 params = params)
  count = count + 1
}

# calculate TWI
find_algorithms(search_term = "(wetnes)")

get_usage("saga:topographicwetnessindextwi")


params = get_args_man(alg = "saga:topographicwetnessindextwi")
params

fs = list.files(path=slope, pattern = "tif$", full.names = TRUE)
fs
fs_catch = list.files(path = catchmant_area, pattern = "tif$", full.names = TRUE)
fs_catch
leng = length(fs)

count = 19

for (i in 19:leng){
  params$SLOPE = fs[i]
  params$METHOD = 0
  params$CONV = 1
  params$AREA = fs_catch[i]
  params$TWI = paste0(twi_image, "twi_", 100 + count)

  out = run_qgis(alg = "saga:topographicwetnessindextwi",
                 params = params)
  count = count + 1
}
