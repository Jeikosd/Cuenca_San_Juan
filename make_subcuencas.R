## 
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(snowfall)
library(stringr)

mkdirs <- function(fp) {
  
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
  
} 



path <- 'data/_basin/'
path_out <- '/raster_to_polygon'

raster_list <- list.files(path, pattern = '*.tif$', full.names = T)
raster_subcuencas <- lapply(raster_list, raster)
raster_names <- lapply(raster_subcuencas, names) %>%
  unlist()

# proof <- rasterToPolygons(raster_subcuencas[[1]], dissolve = T)

sfInit(parallel = TRUE, cpus = 7)
sfExport('raster_subcuencas')
sfExport('dir_out')
sfExport('raster_names')
# sfExport('writeOGR')
sfLibrary(rgeos)
sfLibrary(raster)
sfLibrary(rgdal)
# sfLibrary(snowfall)
shapes_subcuencas <- sfLapply(raster_subcuencas, rasterToPolygons, dissolve = T)

# sfClusterMap(writeOGR, obj = shapes_subcuencas, dsn = dir_out, layer = raster_names, driver = "ESRI Shapefile") probar si creando una funcion sirve
gc()
sfStop() 
gc()

dir_out <- paste0(path, path_out)
mkdirs(dir_out )

Map('writeOGR', obj = shapes_subcuencas, dsn = dir_out, layer = raster_names, driver = "ESRI Shapefile")


subcuencas <- do.call(bind, shapes_subcuencas) 
writeOGR(obj = subcuencas, dsn = dir_out, layer = 'Subcuencas_San_Juan', driver = "ESRI Shapefile")
 