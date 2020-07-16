
rm(list = ls()) 
# 
library(tidyverse)
library(brms)

load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

my_table <- m3 %>% summary() 

my_table$fixed %>% 
  data.frame() %>% 
  mutate( `Fixed Effect` = row.names(. )) %>%
  select( `Fixed Effect`, Estimate:Tail_ESS) %>% 
  pander::pander()

my_table

m3$model

sdev_id <- 
  my_table$random$id %>%
  data.frame() %>% 
  mutate( `Parameter` = row.names(.)) %>%
  mutate( `Group-Level Effect` = 'Participant ID') %>%
  select(`Group-Level Effect`, Parameter, Estimate:Tail_ESS) 

sdev_item <- 
  my_table$random$item %>%
  data.frame() %>% 
  mutate( `Parameter` = row.names(.)) %>%
  mutate(`Group-Level Effect` = 'Image', `Number of Levels` = 131) %>%
  select(`Group-Level Effect`, `Number of Levels`, Parameter, Estimate:Tail_ESS) 

sdev_species <- 
  my_table$random$key %>%
  data.frame() %>% 
  mutate( `Parameter` = row.names(.)) %>%
  mutate( `Group-Level Effect` = 'Species') %>%
  select(`Group-Level Effect`, Parameter, Estimate:Tail_ESS) 

sdev_id

test <- print( my_table )

