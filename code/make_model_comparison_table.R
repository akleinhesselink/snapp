rm(list = ls())
library(tidyverse)
library(brms)

load('output/oos_accuracy.rda')
model_loo_table <- read_csv('output/model_comparison_table.csv')

fit_summary %>% 
  mutate( model_name = paste0( 'm', model)) %>%
  left_join(model_loo_table, by = 'model_name') %>%
  mutate( formula = str_remove( formula, ' \\+ \\(.*\\)')) %>% 
  arrange( looic) %>%
  mutate( delta_looic = looic[1] - looic ) %>% 
  rename( `Candidate Model` = model, `Model Formula` = formula, `R[out]^2` = R2_oo_sample, `R[out*]^2` = R2_oo_sample_no_re, `MEA[out]` = MEA_oo_sample, `MEA[out*]` = MEA_oo_sample_no_re) %>%
  select( `Candidate Model`, delta_looic, looic, se_looic, `R[out]^2`, `R[out*]^2`, `MEA[out]`, `MEA[out*]`, `Model Formula` )  %>% 
  as.data.frame() %>%
  mutate_at( c(2:8), .funs = 'round', 2) %>% 
  write_csv('output/full_model_comparison_table.csv')



