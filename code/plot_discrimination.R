rm(list = ls() )
library(brms)
library(tidyverse)

load('output/fit_2pl.null_key_item.rda')

fit_loo <- brms::loo( fit_null_key_item) 

conditional_effects(fit_null_key_item, 'taxa_repeat',  categorical = T)

summary(fit_null_key_item)

random_fx <- ranef( fit_null_key_item )

key_disc <- random_fx$key[ , , 2] %>% data.frame() %>% mutate( key = rownames(.))
key_int <- random_fx$key[, , 1] %>% data.frame() %>% mutate( key = row.names(.))

item_disc <- random_fx$item[ , , 2] %>% data.frame() %>% mutate( item = rownames(.))
item_int <- random_fx$item[, , 1] %>% data.frame() %>% mutate( item = row.names(.))

#
item_key <- fit_null_key_item$data %>% distinct(item, key)

item_disc_diff <- fit_null_key_item$data %>%
  group_by( item ) %>% 
  summarise( score_avg = mean(as.numeric(score ))) %>% 
  left_join( item_disc ) %>%
  left_join( item_int, by = 'item' ) 

item_low_hi <- item_disc_diff %>%  
  filter( Estimate.x > quantile(Estimate.x, 0.995 ) | Estimate.x < quantile( Estimate.x, 0.005 ) ) %>% 
  left_join(item_key, by = 'item')

item_disc_diff %>% 
  ggplot( aes( x = score_avg, y = Estimate.x )) + 
  geom_point()  + 
  ggrepel::geom_text_repel(data = item_low_hi, aes( label = paste( item, key) )) + 
  ylab('item discrimination') + 
  xlab('average score')

item_disc_diff %>% 
  ggplot( aes( x = score_avg, y = Estimate.y )) + 
  geom_point()  + 
  ggrepel::geom_text_repel(data = item_low_hi, aes( label = paste( item, key) )) + 
  ylab('difficuty') + 
  xlab('average score')

key_disc_diff <- fit_null_key_item$data %>%
  group_by( key ) %>% 
  summarise( score_avg = mean(as.numeric(score ))) %>% 
  left_join( key_disc ) %>%
  left_join( key_int, by = 'key' ) 

key_low_hi <- key_disc_diff %>%  
  filter( Estimate.x > quantile(Estimate.x, 0.95 ) | Estimate.x < quantile( Estimate.x, 0.05 ) )

key_disc_diff %>% 
  ggplot( aes( x = score_avg, y = Estimate.x )) + 
  geom_point()  + 
  ggrepel::geom_text_repel(data = key_low_hi, aes( label = key)) + 
  ylab('item discrimination') + 
  xlab('average score')

item_disc_diff %>% 
  ggplot( aes( x = score_avg, y = Estimate.y )) +
  ggrepel::geom_text_repel(data = key_low_hi, aes( label = key)) + 
  geom_point()  + 
  ylab('difficuty') + 
  xlab('average score')


rm(fit_null_key_item) 
load('output/full_model_fit_1.rda')
m1 <- temp_fit
load( 'output/full_model_fit_2.rda')
m2 <-temp_fit

m1 <- add_criterion(m1, 'loo')
m2 <- add_criterion(m2, 'loo')
loo(m1)
loo_compare(list(loo(m1), loo(m2)))

formula(m1)
formula(m2)


