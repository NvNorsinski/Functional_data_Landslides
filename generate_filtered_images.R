# generate filtered images with different size of moving window
library(RQGIS)

# define input image
p_in = "Daten/Paldau/Parameters/slope_1m.tif"
path_to_image = normalizePath(path = p_in, winslash = "\\", mustWork = NA)

# define output image path
p_out = "Daten/Paldau/Parameters/Filtered_images/"
path_to_out = normalizePath(path = p_out, winslash = "\\", mustWork = NA)

# define output image name
name = "slope_1m"

# set path to qgis installation
set_env("C:/Program Files/QGIS 2.18")

# define size of moving windows for box filtering
seq_radius = seq(from = 1, to = 2, by = 1)
seq_radius2 = seq(from = 20, to = 50, by= 5)
append(seq_radius, seq_radius2)


get_usage(alg = "grass7:r.resamp.filter")
params = get_args_man(alg = "grass7:r.resamp.filter")
params
params$input = path_to_image
params$filter = "box"


for(i in seq_radius){
  params$radius = i
  params$output = paste0(path_to_out, name, i)
  out = run_qgis(alg = "grass7:r.resamp.filter",
                 params = params)


}



