## calcular el promedio zonal
library(tidyverse)
library(raster)
library(sf)
library(sp)

path <- 'data/' 

result_raster <- paste0(path, 'summaries')

cuenca <- st_read(dsn = paste0(path, 'mask/cuencas_finalOK.shp'), layer = 'cuencas_finalOK') 

sum_raster <- list.files(result_raster, pattern = "sum.asc$", full.names = T) %>%
  lapply(raster)

out_dir <- 'data/summaries/'

extract_by_polygon <- function(r, shape, out_dir){
  
  # r <- sum_raster[[1]]
  # shape <- cuenca
  
  
  var_raster <- names(r) 
  r <- st_as_sf(as(r, "SpatialPointsDataFrame"))
  r <- st_set_crs(r, st_crs(shape))
  intersect_cuenca <- st_intersection(r, shape)
  #
  intersect_cuenca_df <- as_data_frame(intersect_cuenca)
  
  sum <- intersect_cuenca_df %>%
    group_by(NameFinal) %>%
    summarize_(sum = paste0('sum(', var_raster, ')')) %>%
    ungroup() %>%
    rename_(.dots = setNames("sum", var_raster))
  
  write_csv(sum, paste0(out_dir, var_raster, ".csv"))
  return(sum)
  
}


lapply(sum_raster, extract_by_polygon, cuenca, out_dir)
