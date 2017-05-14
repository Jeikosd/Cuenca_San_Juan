mkdirs <- function(fp) {
  
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
  
} 

## realizar zonales por variables individuales

library(tidyverse)
library(stringr)
library(gtools)
library(raster)
library(sf)
library(purrr)
library(magrittr)
library(doFuture)
library(compiler)
library(foreach)

avg_by_each_var <- function(variable, dirs_raster, final_name, shape, out_dir){
  
  
  # variable <- 'ActEvap'
  # dirs_raster <- dirs_raster
  # final_name <- '_cuenca.asc'
  # out_dir <- 'data/summaries/'
  # shape <- cuenca
  
  filter_var <- data_frame(text = dirs_raster) %>%
    filter(grepl(variable, text)) %>%
    magrittr::extract2(1) %>%
    mixedsort()
  
  
  # name_raster <- basename(filter_var)
  
  name_raster <- list.files(filter_var, pattern = final_name)
  all_raster <- paste0(filter_var, '/', name_raster)
  
  suppressWarnings(mkdirs(out_dir))
  
  list_raster <- lapply(all_raster, raster) %>%
    stack()
  
  # library(doSNOW)
  registerDoFuture()
  plan(multiprocess)
  
  make_df_by_polygon <- function(r, shape){
    
    
    # r <- list_raster
    var_raster <- names(r)
    out_csv <- gsub("([0-9]+).*$", "", var_raster)[1]
    r <- st_as_sf(as(r, "SpatialPointsDataFrame"))
    r <- st_set_crs(r, st_crs(shape))
    
    # intersect by each polygon?
    
    avg <- foreach(i = 1:dim(shape)[1], .packages = c('sf', 'tidyverse')) %dopar% {
      
      st_intersection(r, filter(shape, row_number() == i)) %>%
        as_data_frame() %>%
        dplyr::select_(.dots = c('NameFinal', paste0(var_raster))) %>%
        group_by(NameFinal) %>%
        summarize_each(funs(mean(.)))%>%
        ungroup() %>%
        rename_(.dots = setNames(paste0(var_raster), paste0('avg_', var_raster)))
      
    }
    
    avg <- bind_rows(avg)
    
   

    write_csv(avg, paste0(out_dir, out_csv, ".csv"))
    
  }
  
  ## each raster to dataframe with shapefile
  
  compiler_fn <- cmpfun(make_df_by_polygon)
  
  compiler_fn(list_raster, shape)
  
  
}



path <- 'data/'


dirs_raster <- list.dirs(paste0(path, 'waterworld_weekly'), full.names = T, recursive = F) 


vars <- gsub("([0-9]+).*$", "", dirs_raster) %>%
  unique() %>%
  basename()


cuenca <- st_read(dsn = paste0(path, 'mask/cuencas_finalOK.shp'), layer = 'cuencas_finalOK') 

final_name <- '_cuenca.asc'  ## ending of raster name
out_dir <- 'data/summaries/' ## folder of results



foreach(i = 1:length(vars)) %do% {
  
  avg_by_each_var(vars[i], dirs_raster, final_name, cuenca, out_dir)
  
}
