rm(list = ls())
library(tidyverse)
library(brms)

load('output/oos_accuracy.rda')
load('output/oos_scored.rda')
model_loo_table <- read_csv('output/model_comparison_table.csv')


oos_accuracy_df  %>% 
  mutate( re = paste0(re, '_RMSE')) %>% 
  spread( re , out_of_sample_RMSE) %>% 
  mutate( model = paste0('m', model)) %>% 
  left_join( model_loo_table %>% 
               rename( model = model_name ), by = 'model') %>% 
  arrange( looic) %>% 
  write_csv('output/full_model_comparison_table.csv')

