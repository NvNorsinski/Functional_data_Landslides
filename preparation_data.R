# put landslide and non Landslide data in one Dataframe
rm(list = ls(all = TRUE))
library(sf)

# TODO
# rechange numbering of colums

# read in data
slope_landsld = readRDS(file = "Daten/Paldau/Samples/filtered_twi_pointVal.rds")


slope_no_landsld = readRDS(file = "Daten/Paldau/Samples/rnd_no_lsd_Paldau_twi.rds")

# output path
responset_path = "Daten/Paldau/Samples/response.rds"
variable_path = "Daten/Paldau/Samples/twi.rds"

# data wrangling section--------------------------------------------------------
seq_radius = seq(from = 1, to = 15, by = 1)
seq_radius2 = seq(from = 20, to = 50, by= 5)
seq_radius = append(seq_radius, seq_radius2)


# renaming function for rows and colums
renam_rs_lsc = function(data, seq_radius, naming_offset = 0){
  rows = nrow(data)
  posseq =  seq(from = 1, to = rows, by = 1)
  cols = ncol(data)
  name = rep("pos: ", rows)
  ns = paste0(name, posseq + naming_offset)
  #colnames(data) = seq_radius
  colnames(data) = seq(from = 1, to = cols, by = 1)
  rownames(data) = ns
  return(data)
}



formating = function(lsd_data, no_lsd_data){
  # Landslide Data handling
  # remove spatial data
  lsd_data = st_drop_geometry(lsd_data)
  # and remove id colum
  lsd_data = lsd_data[c(-1)]
  lsd_data = renam_rs_lsc(lsd_data, seq_radius)
  lsd_data = t(as.matrix(lsd_data))

  # no Landslide data handling

  naming_offset = ncol(lsd_data)
  no_lsd_data = no_lsd_data[-1,]
  no_lsd_data = t(no_lsd_data)
  no_lsd_data = renam_rs_lsc(no_lsd_data, seq_radius, naming_offset)
  no_lsd_data = t(no_lsd_data)
  dat = cbind(lsd_data, no_lsd_data)

  # create response variable-------------------------------------------------------

  response_yes = ncol(lsd_data)
  response_no = ncol(no_lsd_data)

  responset_var = c(rep(1, response_yes), rep(0, response_no))
  posseq =  seq(from = 1, to =  response_no + response_yes)
  names = paste0("pos: " , posseq)
  names(responset_var) = c(names)

  out = list()
  out$responset = responset_var
  out$dat = dat

  return(out)
}

dgm_dat = formating(slope_landsld, slope_no_landsld)
responset = dgm_dat$responset
dat = dgm_dat$dat

#saveRDS(responset, responset_path)
saveRDS(dat, variable_path)
