rm(list = ls()) 
library(tidyverse)

taxa <- read_csv('raw_data/species incl mivs for Andy out.csv')
snake <- read_csv('output/cleaned_response_data.csv')

# Very few families with both MIVS and non-MIVS 
snake %>%
  group_by(key_family, key_mivs ) %>% 
  summarise( n_key = n_distinct(key) ) %>%
  spread( key_mivs, n_key )

snake %>% 
  filter( as.numeric( str_detect(filename, '\\d+' )) < 1001 ) %>% 
  group_by( key_family, key_mivs ) %>% 
  summarise( n_key = n_distinct(key) ) %>%
  spread( key_mivs, n_key, fill = 0  )

taxa %>% 
  group_by( family, mivs ) %>%
  summarise(  n_taxa = n_distinct( binomial)) %>%
  spread( mivs, n_taxa, fill = 0  ) %>% 
  rowwise() %>% 
  mutate( fraction_mivs = yes/(no + yes + unknown)) %>% 
  arrange( desc(fraction_mivs))