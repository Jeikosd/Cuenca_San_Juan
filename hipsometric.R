## curva 


library(tidyverse)
library(raster)
library(rgdal)
library(sf)
library(tmaptools)
library(magrittr)
library(Hmisc)
library(stringr)
library(gsubfn)


path <- 'data/'
masks <-  'mask/'
out <- paste0(path, 'summaries')
  
dem <- raster(paste0(path, masks, 'dem90m_rsm_cuenca_prj.tif'))


cuencas <- st_read(dsn = paste0(path, masks, 'cuencas_finalOK_prj.shp'))



mark_class <- function(x){
  
  # proof
  # x <- '(100,150]'
  # require(gsubfn)
  # require(tidyverse)
  x <- as.character(x)
  interv <- strapply(x, "[[:digit:].]+", as.numeric, simplify = TRUE)
  
  # class_interv <- read.pattern(text = x, pattern = ".(.+),(.+).", col.names = c("lower", "upper")) 
  return(mean(c(interv[1, ], interv[2, ])))
    
}

seqil <- function(from, to, by, include.last = TRUE) {
  x <- do.call(seq.default, list(from, to, by))
  if(include.last) c(x, to) else x
}



# proof_cuenca <- cuencas %>%
  # filter(row_number() == 1)


hipsometrica <- function(dem, cuenca, out){
  
  
  # proof 
  
  # cuenca <- proof_cuenca
  
  dem_df <- as_data_frame(dem@data@attributes[[1]])
  
  maximo <- max(dem_df$Value)
  minimo <- min(dem_df$Value)
  
  name_cuenca <- as.character(magrittr::extract2(cuenca, 'NameFinal'))
  print(name_cuenca)
  dem_cuenca <- crop_shape(dem, cuenca, polygon = T)
  
  # dem_cuenca[][which(!is.na(dem_cuenca[]))]
  
  dem_points <- as_data_frame(rasterToPoints(dem_cuenca)) %>%
    left_join(dem_df, by = c('dem90m_rsm_cuenca_prj' = 'Value'))
  
  
  reclass_dem <- dem_points %>%
    mutate(reclass = cut(dem90m_rsm_cuenca_prj, seqil(from = minimo - 1, to = maximo, by = 50), dig.lab = 10)) 
    # filter(row_number() %in% 1:20) %>%
    # lapply(levels(reclass_dem$reclass), FUN = mark_class)
    # mutate(marca = reclass[-1] - diff(reclass)/2)
    # mutate(mark_class = findInterval(dem90m_rsm_cuenca_prj, seq(minimo, maximo, by = 50), all.inside = T))
    # mutate(mark_var = mark_class(reclass))
    # mutate(mark_var = map(reclass, mark_class)) %>%
    # unnest()
    
    # mutate(data = map(reclass, read.pattern, pattern = ".(.+),(.+)."))
    # mutate(data = map(reclass, read.pattern, pattern = ".(.+),(.+).", col.names = c("lower", "upper")))
           
           # mark_class = cut2(dem90m_rsm_cuenca_prj, seq(minimo, maximo, by = 50), levels.mean = TRUE)) 


# read.pattern(text = labs, pattern = ".(.+),(.+).", col.names = c("lower", "upper"))
  
  
  
  hipsometric <- reclass_dem %>%
    group_by(reclass) %>%
    summarise(count = sum(Count)) %>%
    mutate(mark_class = map(reclass, mark_class)) %>%
    unnest() %>%
    arrange(desc(mark_class)) %>%
    mutate(area_ha = count * 8100 /10000) %>%
    ungroup() %>%
    mutate(area_total = (area_ha / sum(area_ha)) * 100,
           acum = cumsum(area_total),
           mark_class = as.double(mark_class)) 
    
    
  # summary(dem_points$dem90m_rsm_cuenca_prj)
  ggplot(hipsometric) +
    geom_line(aes(x = acum, y = mark_class)) +
    labs(x = 'put_x', y = 'put_y') +
    ggtitle('put_tittle')
  
  
  ggsave(paste0(out, '/hipsometric_', name_cuenca, '.pdf'), width = 20, height = 20, units = "cm")
 
  total_hipsometric <- left_join(reclass_dem, hipsometric, by = 'reclass')
 # el valor que hay que devolver es acum para hacer el raster 
  
  # dem_cuenca[][which(!is.na(dem_cuenca[]))]
  dem_cuenca[which(!is.na(dem_cuenca[]))] <- total_hipsometric$acum
  # total_hipsometric
  # proof <- left_join(data_frame(ID = dem_cuenca[][which(dem_cuenca[] == 998)]), total_hipsometric, by = 'ID') %>%
  #   as_data_frame()
  # dem_df$Value <- total_hipsometric$acum
  # dem@data@attributes[[1]]
  
  writeRaster(dem_cuenca, filename = paste0(out, '/hipsometric_', name_cuenca, '.tiff'), overwrite = TRUE)
## metele write raster
  
}


# library(foreach)
# 
# foreach(i = 1:nrow(cuencas)) %do% {
#   
#   print(i)
#   cuenca <- filter(cuencas, row_number() == i)
#   hipsometrica(dem, cuenca, out)
# }



for(i in 1:nrow(cuencas)) {
  
  print(i)
  cuenca <- filter(cuencas, row_number() == i)
  hipsometrica(dem, cuenca, out)
}



