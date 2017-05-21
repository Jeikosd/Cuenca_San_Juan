## calcular el promedio zonal
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(gtools)
library(purrr)
library(magrittr)
library(doFuture)
# library(compiler)
library(foreach)

path <- 'data/' 

result_raster <- paste0(path, 'summaries')

cuenca <- st_read(dsn = paste0(path, 'mask/cuencas_finalOK.shp'), layer = 'cuencas_finalOK') 

vars <- list.files(result_raster, pattern = "avg.asc$", full.names = T) %>%
  str_replace("([[:digit:]]+)", "") %>%
  str_replace("_avg.asc", "") %>%
  unique() %>%
  basename()

out_dir <- 'data/summaries/'

extract_by_polygon <- function(dir_rasters, shape, vars, out_dir){
  
  # dir_rasters <- result_raster
  # shape <- cuenca
  # layer <- 'NameFinal' 
  # i = 1
  
  registerDoFuture()
  plan(multiprocess)
  
  foreach(i = 1:length(vars), .packages = c('sf', 'tidyverse')) %dopar% {
  
  rasters_stacks <- list.files(dir_rasters, pattern = "avg.asc$", full.names = T) %>%
    str_subset(vars[i]) %>%
    mixedsort() %>%
    lapply(raster) %>%
    stack()
  
  # var_raster <- names(r) 
  r <- st_as_sf(as(rasters_stacks, "SpatialPointsDataFrame"))
  r <- st_set_crs(r, st_crs(shape))
  intersect_cuenca <- st_intersection(r, shape)
  #
  intersect_cuenca_df <- as_data_frame(intersect_cuenca)
  
  var_raster <- names(rasters_stacks)
  
  avg <- intersect_cuenca_df %>%
    group_by(NameFinal) %>%
    select_(.dots = var_raster) %>%
    summarize_each(funs(mean(., na.rm = T))) %>%
    ungroup() %>%
    gather(variable, value, -NameFinal)
  
  
  write_csv(avg, paste0(out_dir, vars[i], "_summaries_avg.csv"))
  
  ##
  rasters_stacks <- list.files(dir_rasters, pattern = "sum.asc$", full.names = T) %>%
    str_subset(vars[i]) %>%
    mixedsort() %>%
    lapply(raster) %>%
    stack()
  
  # var_raster <- names(r) 
  r <- st_as_sf(as(rasters_stacks, "SpatialPointsDataFrame"))
  r <- st_set_crs(r, st_crs(shape))
  intersect_cuenca <- st_intersection(r, shape)
  #
  intersect_cuenca_df <- as_data_frame(intersect_cuenca)
  
  var_raster <- names(rasters_stacks)
  
  avg <- intersect_cuenca_df %>%
    group_by(NameFinal) %>%
    select_(.dots = var_raster) %>%
    summarize_each(funs(mean(., na.rm = T), sum(., na.rm = T))) %>%
    ungroup() %>%
    gather(variable, value, -NameFinal)
  
  write_csv(avg, paste0(out_dir, vars[i], "_summaries_sum.csv"))
  
  }
  
}

extract_by_polygon(result_raster, cuenca, vars, out_dir)


