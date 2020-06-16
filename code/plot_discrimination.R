rm(list = ls() )
library(brms)
library(tidyverse)

load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)
m3$formula

conditional_effects(m3, 'taxa_repeat', conditions = fam_cond,  categorical = T)

hr_cond <- make_conditions(m3, 'home_region')
conditional_effects(m3, 'photo_region', conditions = hr_cond, categorical = T)


fam_cond <- make_conditions(m3, 'key_family')
conditional_effects(m3, 'key_family', fam_cond,  categorical = T)

random_fx <- ranef( m3)
random_fx

key_disc <- random_fx$key[ , , 2] %>% data.frame() %>% mutate( key = rownames(.))
key_int <- random_fx$key[, , 1] %>% data.frame() %>% mutate( key = row.names(.))

item_disc <- random_fx$item[ , , 2] %>% data.frame() %>% mutate( item = rownames(.))
item_int <- random_fx$item[, , 1] %>% data.frame() %>% mutate( item = row.names(.))

#
item_key <- m3$data %>% distinct(item, key)

item_disc_diff <- m3$data %>%
  group_by( item ) %>% 
  summarise( score_avg = mean(as.numeric(score ))) %>% 
  left_join( item_disc ) %>%
  left_join( item_int, by = 'item' ) 

item_disc_diff %>% arrange( desc(score_avg))

m3$formula
stancode(m3)

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
  ylab('easiness') + 
  xlab('average score')

key_disc_diff <- m3$data %>%
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
  ylab('Discrimination') + 
  xlab('average score')

key_disc_diff %>% 
  ggplot( aes( x = score_avg, y = Estimate.y )) +
  ggrepel::geom_text_repel(data = key_low_hi, aes( label = key)) + 
  geom_point()  + 
  ylab('Easiness') + 
  xlab('average score')


summary(m3)

