mkdirs <- function(fp) {
  
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
  
} 
## resumen promedio para cada una de las variables de estudio

average_by_var <- function(var, all_dirs, final_name, out_dir){
  
  require(tidyverse)
  require(stringr)
  require(raster)
  require(gtools)
  require(lubridate)
  require(purrr)
  # var <- 'budget_'
  # all_dirs <- dirs_raster
  
  suppressWarnings(mkdirs(out_dir))
  all_dirs  <- mixedsort(all_dirs)
  
  filter_var <- data_frame(text = all_dirs) %>%
    filter(grepl(var, text)) %>%
    magrittr::extract2(1) %>%
    mixedsort()
  
  # name_raster <- basename(filter_var)
 
  name_raster <- list.files(filter_var, pattern = final_name)
  all_raster <- paste0(filter_var, '/', name_raster)
  
  
  
  list_raster <- lapply(all_raster, raster) %>%
    raster::stack()
  
  avg_raster <- mean(list_raster)
  sum_raster <- sum(list_raster)
  
  writeRaster(avg_raster,
              paste0(out_dir, var, "_avg.asc"),
              format = 'ascii', 
              overwrite = TRUE)
  
  writeRaster(sum_raster,
              paste0(out_dir, var, "_avg.asc"),
              format = 'ascii', 
              overwrite = TRUE)
  
  
  rm(list = setdiff(ls(), c('all_dirs', 'var', 'all_dirs', 'final_name', 'out_dir')))
  gc()
  
  ### by month
  
  
  filter_var <- data_frame(text = all_dirs) %>%
    filter(grepl(var, text)) %>%
    mutate(month = sort(rep(1:12, 4)))

  paste_raster <- function(text, name_raster){
    
    paste0(text, "/", name_raster)
    
  }
  
  sum_raster <- function(r){
    
    sum(r)
  }

  
  raster_tibble <- filter_var %>%
    mutate(name_raster = map(text, list.files, pattern = final_name)) %>%
    mutate(name_raster = map2(text, name_raster, paste_raster)) %>%
    # unnest(name_raster) %>%
    mutate(raster_df = map(name_raster, raster)) %>%
    slice_rows("month") %>%
    by_slice(~stack(.x$raster_df), .to = "month_stack") %>%
    mutate(avg = map(month_stack, mean), 
           sum = map(month_stack, sum_raster))
    
  meses <- month(1:12, label = T)
  
  # map2(raster_tibble$avg, paste0(out_dir, var, meses,"_avg.pdf"), writeRaster, format = 'ascii', overwrite = TRUE)
  # map2(raster_tibble$sum, paste0(out_dir, var, meses,"_sum.pdf"), writeRaster,format = 'ascii', overwrite = TRUE)

  map2(raster_tibble$avg, paste0(out_dir, var, raster_tibble$month,"_avg.pdf"), writeRaster, format = 'ascii', overwrite = TRUE)
  map2(raster_tibble$sum, paste0(out_dir, var, raster_tibble$month,"_sum.pdf"), writeRaster,format = 'ascii', overwrite = TRUE)
}




library(tidyverse)
library(stringr)
library(gtools)
library(magrittr)
library(raster)
library(lubridate)
library(purrr)



path <- 'data/'
out_dir <- 'data/summaries/'

final_name <- '_cuenca.asc'

dirs_raster <- list.dirs(paste0(path, 'waterworld_weekly'), full.names = T, recursive = F) 
# mixedsort(dirs_raster)

# basename(dirs_raster)
# gsub("/", "", "data/waterworld_weekly//ActEvap0001", perl = TRUE)

# grep("ActEvap", mixedsort(dirs_raster), value = T)

## listar solo las varaibles que deseamos realizar el resumen estadístico
vars <- gsub("([0-9]+).*$", "", dirs_raster) %>%
  unique() %>%
  basename()

# average_by_var(vars[1], dirs_raster, final_name, out_dir)



library(foreach)
library(doSNOW)

cl <- makeCluster(6)
registerDoSNOW(cl)  ## For Windows


length_run <- length(vars)

# pb <- txtProgressBar(max = length_run, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# 
# opts <- list(progress=progress)

foreach(i = 1:length_run, .packages = c('raster', 'tidyverse', 'stringr', 'magrittr', 'lubridate', 'purrr')) %dopar% {
  
  print(i)
  average_by_var(vars[i], dirs_raster, final_name, out_dir)
  
  
}


# close(pb)
stopCluster(cl)
