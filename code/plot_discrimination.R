rm(list = ls() )
library(brms)
library(tidyverse)
load('output/training_testing_data.rda')
load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

# Get posterior samples from random effects: 

random_fx <- ranef( m3)

key_disc <- random_fx$key[ , , 2] %>% data.frame() %>% mutate( key = rownames(.))
key_int <- random_fx$key[, , 1] %>% data.frame() %>% mutate( key = row.names(.))

item_disc <- random_fx$item[ , , 2] %>% data.frame() %>% mutate( item = rownames(.))
item_int <- random_fx$item[, , 1] %>% data.frame() %>% mutate( item = row.names(.))

id_int <- random_fx$id[, , 1] %>% data.frame() %>% mutate( id = rownames(.))

item_key <- m3$data %>% distinct(item, key)

item_disc <- item_disc %>% 
  mutate( item_disc = factor( cut( Estimate, quantile(item_disc$Estimate, c(0, 1/5, 4/5, 1)), include.lowest = T), labels = c('lo', 'med', 'hi')))

key_disc <- 
  key_disc %>% 
  mutate( key_disc = factor( cut( Estimate, quantile(key_disc$Estimate, c(0, 1/5, 4/5, 1)), include.lowest = T), labels = c('lo', 'med', 'hi')))

id_int <- 
  id_int %>% 
  arrange(Estimate) %>%
  mutate( id_q = factor( cut(Estimate, breaks = quantile(Estimate, c(0, 0.10, 0.90, 1)), include.lowest = T), labels = c('Low', 'Medium', 'High')) )

#mu_hat <- posterior_linpred(m3, nsamples = 30) %>% t() %>% rowMeans()

#train$mu_hat <- mu_hat

my_fit <- m3$fit

disc_info <- 
  m3$data %>% 
  select( key, item )  %>% 
  bind_cols( as.data.frame(apply( brms::posterior_linpred(m3, dpar = 'disc', nsamples = 200) , 2, quantile ) %>% t()  )) %>% 
  distinct() %>% 
  arrange( key, item ) %>% 
  group_by( key )  %>%
  mutate( avg_disc = mean(`50%`))

disc_info$key_f <- factor( disc_info$key , levels = unique( disc_info$key[ order( disc_info$avg_disc)]), ordered = T)


disc_info %>%
  ungroup() %>% 
  filter( avg_disc < quantile(avg_disc, 0.05 ) | avg_disc > quantile(avg_disc, 0.95))  %>% 
  distinct(key, avg_disc, item, `50%`) %>% 
  arrange(avg_disc ) %>% View

disc_ranks <- 
  disc_info %>% 
  ungroup() %>% 
  mutate( disc_rank = cut( `50%`, breaks = quantile( `50%`, c(0, 0.05, 0.95, 1)), include.lowest = T, labels = c('Low discrimination taxa','Med. disc.', 'High discrimination taxa') ) ) %>%
  filter( disc_rank != 'Med. disc.') 


train %>% 
  left_join(disc_ranks, by = c('key', 'item')) %>%
  left_join(id_int, by = 'id') %>%
  filter( !is.na(disc_rank)) %>% 
  filter( id_q != 'Medium') %>% 
  rename( 'User knowledge' = id_q) %>% 
  ggplot( aes( x = score, group = `User knowledge`, fill = `User knowledge`)) + 
  geom_histogram(stat = 'count', position = 'dodge') + 
  facet_wrap( ~ disc_rank)  
  

id_int %>%
  mutate( id_ordered = factor( id, levels = id[order(Estimate)], ordered = T)) %>% 
  ggplot(aes( x = id_ordered,  y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_point() + 
  geom_errorbar() + 
  coord_flip() 

disc_I <- fixef(m3)['disc_Intercept', 1]

disc_info %>%
  ggplot( aes( x = disc_info$key_f, y = `50%`)) + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination Parameter') + 
  coord_flip()


key_disc %>% 
  mutate(key_order = factor(key, levels = key[order(Estimate)], ordered = T)) %>%
  mutate( Estimate = Estimate + disc_I, `Q2.5` = `Q2.5` + disc_I, `Q97.5` = `Q97.5` + disc_I) %>%
  ggplot( aes( x = key_order, y = Estimate)) + 
  geom_point( aes( y = Estimate ), color = 'blue') + 
  geom_errorbar( aes( ymin = `Q2.5` , ymax = `Q97.5`), alpha = 0.5, color = 'blue') + 
  coord_flip()




