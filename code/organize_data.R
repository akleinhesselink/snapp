rm(list = ls())
library(tidyverse)

user_info <- 
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
  distinct(user_id, user_region )

key_info <- 
  read_csv(file = 'models for Andy/snapp_responses_out-corr-only100spp-toErol.csv') %>% 
  distinct(filename, species, global_region, mivs, difficulty) 

user_question_seq <- 
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
  distinct(user_id, filename, seq  ) 

response_list <- 
  read_csv('models for Andy/DT.sm100.csv') %>% 
  distinct( answer )

species_mivs <- 
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
  distinct(answer, answer.mivs) %>% 
  filter( !is.na(answer.mivs)) %>% 
  filter( str_detect( answer, ' ')) %>% 
  distinct() 

genus_mivs <- 
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
  distinct(answer.genus, answer.mivs.g) %>% 
  filter( !is.na(answer.mivs.g)) 

family_mivs <- 
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
  distinct(answer.family, answer.mivs.f) %>% 
  filter( !is.na(answer.mivs.f)) 

# Classify each family as mivs (y/n/ambiguous)
bind_rows(
  read_csv('models for Andy/snapp_responses_out-corr-only100spp-toErol.csv') %>% 
    select( family, mivs ) %>% 
    distinct()  %>% 
    arrange( family ), 
  
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
    select(answer.family , answer.mivs.f) %>% 
    filter( !is.na( answer.family)) %>% 
    distinct() %>% 
    arrange( answer.family) %>% 
    rename( family = answer.family, mivs = answer.mivs.f)
  ) %>% 
  distinct( family, mivs ) %>% 
  arrange( family ) %>% 
  group_by( family ) %>% 
  mutate( mivs = ifelse( n() > 1, "ambiguous", mivs )) %>% 
  distinct( family, mivs )


bind_rows(
  read_csv('models for Andy/snapp_responses_out-corr-only100spp-toErol.csv') %>% 
    select( genus, mivs ) %>% 
    distinct()  %>% 
    arrange( genus ), 
  
  read_csv(file = 'models for Andy/DT.sm100.csv') %>% 
    select(answer.genus , answer.mivs.g) %>% 
    filter( !is.na( answer.genus)) %>% 
    distinct() %>% 
    arrange( answer.genus) %>% 
    rename( genus = answer.genus, mivs = answer.mivs.g)
) %>% 
  distinct( genus, mivs ) %>% 
  arrange( genus ) %>% 
  group_by( genus ) %>% 
  mutate( mivs = ifelse( n_distinct(mivs, na.rm = T) == 2, "ambiguous", mivs )) %>% 
  distinct( genus, mivs ) %>% View

 

mivs_info <- 
  species_mivs %>% 
  rename( taxa = answer, mivs = answer.mivs) %>% 
  mutate( level = 'binomial') %>% 
  bind_rows( 
    genus_mivs %>% 
      rename( taxa = answer.genus, mivs = answer.mivs.g) %>% 
      mutate( level = 'genus')
    ) %>% 
  bind_rows(
    family_mivs %>% 
      rename( taxa = answer.family, mivs = answer.mivs.f) %>% 
      mutate( level = 'family')
    )

snake <- read_csv(file = 'models for Andy/snapp_responses_out-corr-only100spp-toErol.csv')
ReptileDatabase <- read_delim('~/Downloads/RD-accepted-species.csv', delim = ';')
ReptileDatabase2018 <- readxl::read_xlsx('~/Downloads/Reptile_checklist_2018_07.xlsx')

herp_names <- 
  ReptileDatabase2018 %>% 
  mutate( binomial  =  Species ) %>% 
  separate( Species, c('genus', 'species') ) %>% 
  select( - species ) %>% 
  separate( Familyetc, c('family', 'superf', 'x1', 'x2', 'order'), sep = ', ') %>% 
  select( binomial, genus, family) %>% 
  mutate( family = str_extract(family, '^[A-Za-z]+')) %>%
  distinct() %>% 
  mutate( response_family = family, response_genus = genus) %>% 
  gather( response_level, taxa , c(family, genus, binomial)) %>% 
  distinct( response_level, response_family, response_genus, taxa ) %>% 
  mutate( response_genus = ifelse( response_level == "family", NA, response_genus )) %>% 
  distinct( )

herp_names2 <- 
  ReptileDatabase %>%
  mutate( binomial = str_replace( AcceptedTaxonID,  '_', ' ') ) %>%
  distinct( binomial, Genus, Family ) %>%
  mutate( family = Family , genus = Genus) %>%
  rename( response_family = Family, response_genus = Genus ) %>%
  gather( response_level, taxa , c(family, genus, binomial)) %>%
  distinct(response_family, response_genus, taxa, response_level ) %>% 
  filter( !(taxa %in% unique( herp_names$taxa) )) %>% 
  mutate( response_genus = ifelse( response_level == "family", NA, response_genus )) %>% 
  distinct( )

herp_names <- 
  bind_rows(herp_names, herp_names2 ) %>% 
  distinct( ) 

snake <- 
  snake %>% 
  rename( key_family = family , 
          key_genus = genus, 
          key_binomial = species, 
          response = answer ) 

#### Bad response taxa --- don't match Reptile Database Names 
snake %>% filter( response == 'Dipsas turgidus') 
snake %>% filter( response == 'Gongylosoma baliodeirum' ) 

ReptileDatabase[ ReptileDatabase$Genus == 'Dipsas', ]  
ReptileDatabase[ ReptileDatabase$Genus == 'Gongylosoma', ]
ReptileDatabase2018[ str_detect( ReptileDatabase2018$Species, 'Gongylosoma' ) , ] 

# make replacements 
snake <- 
  snake %>% 
  mutate( response =  str_replace_all(response, 
                                      c('Gongylosoma baliodeirum' = 'Gongylosoma baliodeirus', 
                                        'Dipsas turgidus' = 'Dipsas turgida'))) %>% 
  left_join( herp_names, by = c('response' = 'taxa')) 

# Categorize user responses: 

snake <- 
  snake %>% 
  mutate( skip = ifelse( is.na(response), 1, skip )) %>% 
  mutate( response_level = ifelse( skip == 1, "skip", response_level))  %>% 
  mutate( response_binomial = ifelse(response_level == 'binomial', response, NA)) %>% 
  mutate( response_family = replace_na(response_family, "skip" )) %>% 
  mutate( response_genus = replace_na( response_genus, "skip")) %>% 
  mutate( response_binomial = replace_na( response_binomial, "skip")) %>% 
  mutate( family_correct = response_family == key_family, 
          genus_correct = response_genus == key_genus, 
          binomial_correct = response_binomial == key_binomial) 


snake %>% 
  group_by( key_family ) %>% 
  summarise( skipped = mean( skip ), 
             family_correct = mean(family_correct), 
             genus_correct = mean(genus_correct), 
             binomial_correct = mean(binomial_correct)) %>% 
  gather( level, prop, skipped:binomial_correct) %>% 
  mutate( level = factor( level, levels = c('skipped', 'family_correct', 'genus_correct', 'binomial_correct'), ordered = T)) %>% 
  ggplot( aes( x = key_family, y = prop, fill = level )) + 
    geom_bar( stat = 'identity', position = 'dodge')
  
snake %>% 
  group_by( difficulty ) %>% 
  summarise( skipped = mean( skip ), 
             family_correct = mean(family_correct), 
             genus_correct = mean(genus_correct), 
             binomial_correct = mean(binomial_correct)) %>% 
  gather( level, prop, skipped:binomial_correct) %>% 
  mutate( level = factor( level, levels = c('skipped', 'family_correct', 'genus_correct', 'binomial_correct'), ordered = T)) %>% 
  ggplot( aes( x = difficulty, y = prop, fill = level )) + 
  geom_bar( stat = 'identity', position = 'dodge')


overall_error <- 
  snake %>% 
  left_join(mivs_info %>% rename(response_mivs = mivs), by = c('response' = 'taxa', 'response_level' = 'level')) %>% 
  mutate( type = ifelse( mivs == response_mivs, 'correct', NA)) %>% 
  mutate( type = ifelse( mivs == 'n' & response_mivs == 'v', 'false positive', type)) %>% 
  mutate( type = ifelse( mivs == 'v' & response_mivs == 'n', 'false negative', type )) %>% 
  mutate( type = ifelse( is.na( response_mivs), 'uncategorizied', type )) %>% 
  group_by( type ) %>% 
  summarise( n =  n() ) %>% 
  ungroup() %>% 
  mutate( p = n/ sum(n))

error_by_region <- 
  snake %>% 
  left_join(mivs_info %>% rename(response_mivs = mivs), by = c('response' = 'taxa', 'response_level' = 'level')) %>% 
  filter( !skip ) %>% 
  mutate( type = ifelse( mivs == response_mivs, 'correct', NA)) %>% 
  mutate( type = ifelse( mivs == 'n' & response_mivs == 'v', 'false positive', type)) %>% 
  mutate( type = ifelse( mivs == 'v' & response_mivs == 'n', 'false negative', type )) %>% 
  mutate( type = ifelse( is.na( response_mivs), 'uncategorizied', type )) %>% 
  group_by( global_region, type ) %>%  
  summarise( n = n()  ) %>% 
  group_by( global_region ) %>% 
  mutate( p = n/ sum(n) ) 

error_by_region %>% 
  ggplot( aes(x = global_region, y = p , fill = type )) + geom_bar(stat = 'identity', position = 'dodge') 

overall_error %>% 
  ggplot( aes(x = type, y = p , fill = type )) + geom_bar(stat = 'identity', position = 'dodge')


save(snake, user_info, user_question_seq,  file = 'Data/snake_data.rda')

