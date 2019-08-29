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
tpi = "Daten/Paldau/Parameters/Filtered_images_tpi"
aspect_sincos = "Daten/Paldau/Parameters/Filtered_images_aspectsincos"
normalized_height = "Daten/Paldau/Parameters/Filtered_images_normalizedHeight"


# calculate slope, aspect and curvature
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
  run_qgis(alg = "saga:slopeaspectcurvature", params = params)
  count = count + 1
}




# calculate catchment area
get_usage(alg = "saga:flowaccumulationrecursive")

params = get_args_man(alg = "saga:flowaccumulationrecursive")
params

count = 0
for (i in fs){
  params$ELEVATION = i
  params$STEP = 1
  params$METHOD = 0
  params$CONVERGENCE = 1.1
  params$FLOW = paste0(catchmant_area,"catchmant_area_", 100 + count)
  run_qgis(alg = "saga:flowaccumulationrecursive", params = params)
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

count = 0

for (i in 1:leng){
  params$SLOPE = fs[i]
  params$METHOD = 0
  params$CONV = 1
  params$AREA = fs_catch[i]
  params$TWI = paste0(twi_image, "twi_", 100 + count)
  run_qgis(alg = "saga:topographicwetnessindextwi", params = params)
  count = count + 1
}



# calculate topographic position index (TPI)
get_usage(alg = "gdalogr:tpitopographicpositionindex")

find_algorithms(search_term = "(tpi)")
params = get_args_man(alg = "gdalogr:tpitopographicpositionindex")
params

fs = list.files(path = path_to_dgm, pattern = "tif$", full.names = TRUE)
fs

count = 0
for (i in fs){
  params$INPUT = i
  params$OUTPUT = paste0(tpi, "tpi_", 100 + count)
  run_qgis(alg = "gdalogr:tpitopographicpositionindex", params = params)
  count = count + 1
}

# calculate aspect with units in sin and cos
get_usage(alg = "gdalogr:aspect")

find_algorithms(search_term = "(relative)")
params = get_args_man(alg = "gdalogr:aspect")
params

fs = list.files(path = path_to_dgm, pattern = "tif$", full.names = TRUE)
fs

count = 0
for (i in fs){
  params$INPUT = i
  params$OUTPUT = paste0(aspect_sincos, "tpi_", 100 + count)
  params$TRIG_ANGLE = 1
  run_qgis(alg = "gdalogr:aspect", params = params)
  count = count + 1
}
# normalized hight index- does not work !!!

fs = list.files(path = path_to_dgm, pattern = "tif$", full.names = TRUE)
fs

params = get_args_man(alg = "saga:relativeheightsandslopepositions")
params

count = 0
for (i in fs){
  params$DEM = i
  params$NH = paste0(normalized_height, "nmh_", 100 + count)
  params$`_RESAMPLING` = 3
  run_qgis(alg = "saga:relativeheightsandslopepositions", params = params)
  count = count + 1
}
