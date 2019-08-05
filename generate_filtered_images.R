# generate filtered images with different size of moving window
rm(list = ls(all = TRUE))
library(RQGIS)

# set path to qgis installation. Version 2.x is needed
set_env("C:/Program Files/QGIS 2.18")

# define input image
path_to_image = "Daten/Paldau/Parameters/slope_1m.tif"

# define output image path
path_to_out = "Daten/Paldau/Parameters/Filtered_images_slope/"


# define output image name
name = "slope_1m"


# define sizes of moving windows for filtering
seq_radius = seq(from = 1, to = 15, by = 1)
seq_radius2 = seq(from = 20, to = 50, by= 5)
seq_radius = append(seq_radius, seq_radius2)

# confugure the process
get_usage(alg = "grass7:r.resamp.filter")
params = get_args_man(alg = "grass7:r.resamp.filter")
params
params$input = path_to_image
params$filter = "box"

count = 0
for(i in seq_radius){

  params$radius = i
  params$output = paste0(path_to_out, name,"_", 100 + count)
  out = run_qgis(alg = "grass7:r.resamp.filter",
                 params = params)
  count = count + 1
}
