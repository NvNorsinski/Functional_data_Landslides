# generate filtered images with different size of moving window
rm(list = ls(all = TRUE))
library(RQGIS)

# set path to qgis installation. Version 2.x is needed
set_env("C:/Program Files/QGIS 2.18")

# define input image
path_to_slope_image = "Daten/Paldau/Parameters/slope_paldau.tif"
path_to_dgm = "Daten/Paldau/Parameters/dgm_1m_AOI.tif"

# define output image path
path_to_out = "Daten/Paldau/Parameters/"


# define output image name
slope = "slope_paldau"
aspect = "aspect_paldau"
general_curvature = "general_curvature_paldau"
plan_curvature = "plan_curvature_paldau"

catchmant_area = "catchmantarea"
twi_image = "TWI"


get_usage(alg = "saga:slopeaspectcurvature")

params = get_args_man(alg = "saga:slopeaspectcurvature")
params

params$ELEVATION = path_to_slope_image
params$SLOPE = paste0(path_to_out, slope)
params$ASPECT = paste0(path_to_out, aspect)
params$C_GENE = paste0(path_to_out, general_curvature)
params$C_PLAN = paste0(path_to_out, plan_curvature)
params$METHOD = 6


out = run_qgis(alg = "saga:slopeaspectcurvature",
                params = params)


# calculate catchment area
get_usage(alg = "saga:flowaccumulationrecursive")

params = get_args_man(alg = "saga:flowaccumulationrecursive")
params

params$ELEVATION = path_to_dgm
params$STEP = 1
params$METHOD = 0
params$CONVERGENCE = 1.1
params$FLOW = paste0(path_to_out, catchmant_area)

# calculate TWI
find_algorithms(search_term = "(wetnes)")

get_usage("saga:topographicwetnessindextwi")


params = get_args_man(alg = "saga:topographicwetnessindextwi")
params

params$SLOPE = ppath_to_slope_image
params$METHOD = 0
params$AREA = 1
params$AREA = paste0(path_to_out, catchmant_area,"tif")
params$TWI = paste0(path_to_out, twi_image)
