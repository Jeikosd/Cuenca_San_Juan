library(raster)
library(tidyverse)
library(sf)
library(tmaptools)

## funciones necesarias
unzip_raster <- function(zipfile, shape) {
  
  require(raster)
  require(tidyverse)
  require(sf)
  require(tmaptools)
  
  # shape <- cuenca
  # zipfile <- names_zip[1]
  
  
  
  name_file <- gsub("\\.zip$", "", zipfile)
  
  dir_out <- file.path(name_file)
  
  dir.create(dir_out)
  
  unzip(zipfile, exdir = dir_out)
  
  shape_sp <- as(shape, "Spatial")
  
  raster_file <- list.files(dir_out, pattern = ".asc$", full.names = T) %>%
    raster() %>%
    crop_shape(shape_sp, polygon = TRUE)
  
  out_file <- list.files(dir_out, pattern = ".asc$") %>%
    gsub("\\.asc$", "", .)
  
  filename = paste0(dir_out, '/', out_file , '_cuenca.asc') 
  
  raster_file <- writeRaster(raster_file,
                             filename,
                             format = 'ascii')
  
  
  return(raster_file)
  
}





path <- 'data/'

names_zip <- list.files(paste0(path, 'waterworld_weekly/'), full.names = T)
cuenca <- st_read(dsn = paste0(path, '/mask/sanJuan1order.shp'), layer = 'sanJuan1order')

library(snowfall)
sfInit(parallel = TRUE, cpus = 3)
sfExport('names_zip')
sfExport('cuenca')
sfExport('path')


sfLibrary(raster)
sfLibrary(tidyverse)
sfLibrary(sf)
sfLibrary(tmaptools)
# sfLibrary(snowfall)
extract_by_mask <- sfLapply(names_zip, unzip_raster, shape = cuenca)

# sfClusterMap(writeOGR, obj = shapes_subcuencas, dsn = dir_out, layer = raster_names, driver = "ESRI Shapefile") probar si creando una funcion sirve
gc()
sfStop() 
gc()


