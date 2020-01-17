# sample non landslide area from raster
rm(list = ls(all = TRUE))
library(raster)
library(sf)
library(sp)
library(rgeos)


files = c("Filtered_images_aspect_ns", "Filtered_images_aspect_ow",
          "Filtered_images_catchmant_area", "Filtered_images_dgm",
          "Filtered_images_generalCurvature", "Filtered_images_planCurvature",
          "Filtered_images_slope", "Filtered_images_tpi", "Filtered_images_twi")

out_names = c("dataframe_Paldau_aspect_ns", "dataframe_Paldau_aspect_ow",
              "dataframe_Paldau_catchmant_area", "dataframe_Paldau_dgm",
              "dataframe_Paldau_generalCurvature",
              "dataframe_Paldau_planCurvature", "dataframe_Paldau_slope",
              "dataframe_Paldau_tpi", "dataframe_Paldau_twi")


# ouput path
path_to_out = "Daten/Paldau/Samples"

# input path



#-------------------------------------------------------------------------------



for (i in 1:length(files)){
  path_to_images = paste0("Daten/Paldau/Parameters/", files[i])


  fs = list.files(path=path_to_images, pattern = "tif$", full.names = TRUE)
  fs
  k = 100

  for (j in fs) {

    img = brick(paste0(j))

    img = aggregate(img, 4)

    mat = as.matrix(img)

    #mat[length,] = mat[length-1,]

    saveRDS(mat, paste0(path_to_out,"/", out_names[i] ,"_",k,".rds"))
    k = k+1

  }
}

