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
  # zipfile <- names_zip[i]
  
  
  
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
cuenca <- st_read(dsn = paste0(path, 'mask/cuencas_finalOK.shp'), layer = 'cuencas_finalOK')



library(foreach)
library(doSNOW)


cl <- makeCluster(3)
registerDoSNOW(cl)  ## For Windows


length_run <- length(names_zip)

# pb <- txtProgressBar(max = length_run, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# 
# opts <- list(progress=progress)

extract_by_mask <- foreach(i = 1:length_run, .packages = c('raster', 'tidyverse', 'sf', 'tmaptools')) %dopar% {
  
  
          unzip_raster(names_zip[i], shape = cuenca) 
  
  
}


# close(pb)
stopCluster(cl)


## unzip_raster(names_zip[153], shape = cuenca) 
## change to foreach

# library(snowfall)
# sfInit(parallel = TRUE, cpus = 3)
# sfExport('names_zip')
# sfExport('cuenca')
# sfExport('path')


# sfLibrary(raster)
# sfLibrary(tidyverse)
# sfLibrary(sf)
# sfLibrary(tmaptools)
# sfLibrary(snowfall)
# extract_by_mask <- sfLapply(names_zip, unzip_raster, shape = cuenca)

# sfClusterMap(writeOGR, obj = shapes_subcuencas, dsn = dir_out, layer = raster_names, driver = "ESRI Shapefile") probar si creando una funcion sirve
# gc()
# sfStop() 
# gc()


