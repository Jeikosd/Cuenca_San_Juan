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
  # var <- 'budget_'
  # all_dirs <- dirs_raster
  # mask <- cuenca
  
  filter_var <- data_frame(text = all_dirs) %>%
    filter(grepl(var, text)) %>%
    magrittr::extract2(1) %>%
    mixedsort()


  # name_raster <- basename(filter_var)
 
  name_raster <- list.files(filter_var, pattern = final_name)
  all_raster <- paste0(filter_var, '/', name_raster)
  
  suppressWarnings(mkdirs(out_dir))
  
  list_raster <- lapply(all_raster, raster) %>%
    raster::stack()
  
  avg_raster <- mean(list_raster)
  
  writeRaster(avg_raster,
              paste0(out_dir, var, "_avg.asc"),
              format = 'ascii', 
              overwrite = TRUE)
  
}



library(foreach)
library(doSNOW)


library(tidyverse)
library(stringr)
library(gtools)
library(magrittr)
library(raster)



path <- 'data/'
out_dir <- 'data/summaries/'

final_name <- '_cuenca.asc'

dirs_raster <- list.dirs(paste0(path, 'waterworld_weekly'), full.names = T) %>%
  .[-1]
# mixedsort(dirs_raster)

# basename(dirs_raster)
# gsub("/", "", "data/waterworld_weekly//ActEvap0001", perl = TRUE)

# grep("ActEvap", mixedsort(dirs_raster), value = T)

## listar solo las varaibles que deseamos realizar el resumen estadístico
vars <- gsub("([0-9]+).*$", "", dirs_raster) %>%
  unique() %>%
  basename()

# average_by_var(vars[1], dirs_raster, final_name, out_dir)

cl <- makeCluster(3)
registerDoSNOW(cl)  ## For Windows


length_run <- length(vars)

# pb <- txtProgressBar(max = length_run, style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# 
# opts <- list(progress=progress)

average_by_var <- foreach(i = 1:length_run, .packages = c('raster', 'tidyverse', 'stringr', 'magrittr')) %dopar% {
  
  
  average_by_var(vars[i], dirs_raster, final_name, out_dir)
  
  
}


# close(pb)
stopCluster(cl)
