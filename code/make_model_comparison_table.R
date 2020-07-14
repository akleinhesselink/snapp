rm(list = ls())
library(tidyverse)
library(brms)

load('output/oos_accuracy.rda')
load('output/oos_scored.rda')
model_loo_table <- read_csv('output/model_comparison_table.csv')


comparison_table <- 
  oos_accuracy_df  %>% 
  mutate( re = paste0(re, '_RMSE')) %>% 
  spread( re , out_of_sample_RMSE) %>% 
  mutate( model = paste0('m', model)) %>% 
  left_join( model_loo_table %>% 
               rename( model = model_name ), by = 'model') %>% 
  arrange( looic) 
  
comparison_table %>%
  mutate( formula = str_remove( formula, ' \\+ \\(.*\\)')) %>% 
  mutate( delta_looic = looic[1] - looic ) %>% 
  rename( `Candidate Model` = model, `Model Formula` = formula, `Out of Sample RMSE 1`= all_RMSE, `Out of Sample RMSE 2` = none_RMSE) %>%
  select( `Candidate Model`, delta_looic, looic, se_looic, `Out of Sample RMSE 1`, `Out of Sample RMSE 2`, `Model Formula`)  %>% 
  as.data.frame() %>%
  mutate_at( c(2:6), .funs = 'round', 2) %>% 
  write_csv('output/full_model_comparison_table.csv')



