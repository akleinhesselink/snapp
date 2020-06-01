rm(list =ls() )
library(tidyverse)
library(brms)

load( file = 'output/training_testing_data.rda')

train %>% 
  rbind(test) %>% 
  group_by( key)  %>% 
  summarise( min_repeats = min( taxa_repeat), max_repeats=  max( taxa_repeat)) %>% 
  write_csv('output/taxa_repeats.csv')

load( 'output/fit_form3.1.rda')
form3.1 <- temp_fit
load( 'output/full_model_fit_1.rda')
form3 <- temp_fit

summary(form3)
plot(form3)

conditions_hr <- make_conditions(form3, 'home_region')
conditions_fam <- make_conditions(form3, 'key_family')

conditional_effects( form3, effects = 'photo_region', categorical = T)
conditional_effects( form3, effects = 'photo_region', conditions = conditions_fam,  categorical = T)

conditional_effects( form3, effects = 'photo_region', conditions = conditions_hr, categorical = T)

conditional_effects( form3.1, effects = 'home_region', conditions = conditions_fam, categorical = T)


conditional_effects( form3.1, effects = 'taxa_repeat', conditions =  conditions_hr,  categorical = T)



form3.1$data %>% View

conditions <- make_conditions(form3.1, 'photo_region')
plot( conditional_effects( form3.1, effects = 'taxa_repeat', conditions =  conditions,  categorical = T))


conditions <- make_conditions(form3.1, 'key_family')
plot( conditional_effects( form3.1, effects = 'taxa_repeat', conditions =  conditions,  categorical = T))

form3.1$








form3.1$prior
form3.2$prior
form3.3$prior

summary(form3.3)
summary(form3.2)

form3.1$fit

plot(form3.1)

conditional_effects(form3.1, effects = 'taxa_repeat', categorical = T)

library(rstan) 

ranef( form3.1 ) 

test <-  form3.1$fit  
test@model_pars



item_info <- 
  form3.1$data %>%  
  #mutate(item = as.numeric( factor( item)) ) %>% 
  distinct(item, key_family, photo_region, key)

length( item_info$item )
max(item_info$item)

disc <- 
  test %>% 
  as.data.frame() %>% select( starts_with('r_item')) %>% 
  mutate( iter = row_number()) %>%
  gather( item, disc, starts_with('r_item')) %>%
  mutate( item = as.numeric( str_extract(item, '\\d+'))) %>% 
  left_join( item_info, by = 'item')

disc %>% 
  ggplot( aes( x = disc, fill = photo_region)) + 
            geom_histogram() + 
            facet_grid( photo_region ~ .  )

disc %>% 
  ggplot( aes( x = disc, fill = key_family)) + 
  geom_histogram() + 
  facet_grid(key_family ~ . )

disc %>% 
  ggplot( aes( x = disc, fill = key_family)) + 
  geom_histogram() + 
  facet_wrap(key_family ~ photo_region , scales = 'free_y')



summary( form3.1)