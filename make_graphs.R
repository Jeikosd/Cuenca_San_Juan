## librerias Necesarias
library(tidyverse)
library(stringr)
library(lubridate)
library(sf)
library(forcats)
##
path <- 'data/' 
out_graphs <- 'data/summaries/'
title <- 'Time series of'

summaries_csv <- list.files(paste0(path, 'summaries'), pattern = ".csv$", full.names = T)

names_var <- str_replace(basename(summaries_csv), '.csv', '') %>%
  str_replace('summaries_', '')

mask_shp <- st_read(dsn = paste0(path, 'mask/cuencas_finalOK.shp')) %>%
  as_data_frame() %>%
  select(NameFinal, cluster)


summaries_df <- map(summaries_csv, read_csv)

for(i in 1:length(names_var)){
  
  print(i)
  proof <- summaries_df[[i]]
  variable <- names_var[i]
  condition <- str_extract(variable, 'sum')
  
  proof <- left_join(proof, mask_shp, by = 'NameFinal') %>%
    mutate(cluster  = factor(cluster))
  
  # proof %>%
  #   group_by(variable, cluster) %>%
  #   summarise(avg_cluster = mean(value))
  
  if(is.na(condition)){
    
    proof_graphs <- proof %>%
      filter(str_detect(variable, "[:digit:]")) %>%
      group_by(NameFinal) %>%
      mutate(months = month(1:12, label = T)) %>%
      ungroup() %>%
      group_by(months, cluster) %>%
      summarise(avg_cluster = mean(value)) %>%
      ungroup()
    
    ggplot(proof_graphs) + 
      geom_line(aes(months, avg_cluster, colour = cluster, group = cluster)) +
      theme_bw() +
      ylab(variable) + 
      ggtitle(paste(title, variable))
    
    ggsave(paste0(out_graphs, variable, '.pdf'), height = 7, width = 17, units= 'in', dpi = 600)
    
  } else{
    
    proof_graphs <- proof %>%
      filter(str_detect(variable, "[:digit:]")) %>%
      group_by(NameFinal) %>%
      mutate(months = rep(month(1:12, label = T), 2), type = sort(rep(c('avg', 'sum'), 12))) %>%
      ungroup() %>%
      group_by(type, months, cluster) %>%
      summarise(avg_cluster = mean(value)) %>%
      ungroup()
    
    proof_graphs %>%
      filter(type == 'avg') %>%
      ggplot() + 
      geom_line(aes(months, avg_cluster, colour = cluster, group = cluster)) +
      theme_bw() +
      ylab(variable) + 
      ggtitle(paste(title, variable, 'average')) 
      
    ggsave(paste0(out_graphs, variable, '_mean.pdf'), height = 7, width = 17, units= 'in', dpi = 600)
    
    proof_graphs %>%
      filter(type == 'sum') %>%
      ggplot() + 
      geom_line(aes(months, avg_cluster, colour = cluster, group = cluster)) +
      theme_bw() +
      ylab(variable) + 
      ggtitle(paste(title, variable, 'sum')) 
    
    ggsave(paste0(out_graphs, variable, '_sum.pdf'), height = 7, width = 17, units= 'in', dpi = 600)
  }
  
 
  
  # ggplot(proof_graphs) + 
  #   geom_line(aes(months, avg_cluster, colour = cluster, group = cluster)) +
  #   theme_bw() +
  #   ylab(variable) + 
  #   ggtitle(paste(title, variable))
  # 
  # ggsave(paste0(out_graphs, variable, '.pdf'), height = 7, width = 17, units= 'in', dpi = 600)
  # 
  
}

