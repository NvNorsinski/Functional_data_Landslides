# generte point on surface for landslidepolygons
rm(list = ls(all = TRUE))
library(RQGIS)

# set path to qgis installation. Version 2.x is needed
set_env("C:/Program Files/QGIS 2.18")

# define input polygon
p_in = "Daten/Paldau/Landslides/Landslides_source.shp"
path_to_shape = normalizePath(path = p_in, winslash = "\\", mustWork = NA)

# define output path
p_out = "Daten/Paldau/Landslides/"
path_to_out = normalizePath(path = p_out, winslash = "\\", mustWork = NA)

# define output name
name = "Paldau_source_point"

# configure the process
get_usage(alg = "qgis:pointonsurface")
params = get_args_man(alg = "qgis:pointonsurface")
params

params$INPUT_LAYER = path_to_shape
params$OUTPUT_LAYER = paste0(path_to_out, name)

out = run_qgis(alg = "qgis:pointonsurface",
               params = params)

