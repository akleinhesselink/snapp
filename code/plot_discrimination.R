rm(list = ls() )
library(brms)
library(tidyverse)
load('output/training_testing_data.rda')
load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

ns <- 200  # number of posterior samples
pw <- 8 # image width 
ph <- 8 # image height 

m3 %>% summary

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

my_fit <- m3$fit

disc_info <- 
  m3$data %>% 
  select( key, item )  %>% 
  bind_cols( as.data.frame(apply( brms::posterior_linpred(m3, dpar = 'disc', nsamples = ns) , 2, quantile , probs = c(0.025, 0.5, 0.975)) %>% t()  )) %>% 
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

disc_ranks 

train %>% 
  left_join(disc_ranks, by = c('key', 'item')) %>%
  left_join(id_int, by = 'id') %>%
  filter( !is.na(disc_rank)) %>% 
  filter( id_q != 'Medium') %>% 
  rename( 'User knowledge' = id_q) %>% 
  ggplot( aes( x = score, group = `User knowledge`, fill = `User knowledge`)) + 
  geom_histogram(stat = 'count', position = 'dodge') + 
  facet_wrap( ~ disc_rank)  
  

disc_I <- fixef(m3)['disc_Intercept', 1]

disc_info %>%
  ggplot( aes( x = key_f, y = `50%`)) + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave(filename = 'figures/discrimination_by_taxa_points.png', height = ph, width = pw )


disc_info %>%
  ggplot( aes( x = key_f, y = `50%`)) + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6))

key_disc %>% 
  mutate(key_order = factor(key, levels = key[order(Estimate)], ordered = T)) %>%
  mutate( Estimate = Estimate + disc_I, `Q2.5` = `Q2.5` + disc_I, `Q97.5` = `Q97.5` + disc_I) %>%
  ggplot( aes( x = key_order, y = Estimate)) + 
  geom_point( aes( y = Estimate ), color = 'blue') + 
  geom_errorbar( aes( ymin = `Q2.5` , ymax = `Q97.5`), alpha = 0.5, color = 'blue') + 
  coord_flip() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  theme(axis.text.y = element_text(face = 'italic', size = 6))  + 
  ggsave(filename = 'figures/discrimination_by_taxa.png', height = ph, width = pw )


ref_taxa <- "Opheodrys aestivus"
disc_info %>% 
  left_join( train %>% ungroup() %>% distinct(item, difficulty, key, key_family) , by = c('key','item')) %>% 
  filter( key == ref_taxa)  %>% 
  mutate( item_f = factor( item, levels = unique(item[order(`50%`)]),ordered = T) ) %>% 
  ggplot( aes( x = item_f, y = `50%`, ymin = `2.5%`, ymax = `97.5%`, color = difficulty) ) + 
  geom_point() + 
  geom_errorbar()+ 
  coord_flip() + 
  ggtitle( paste0( ref_taxa ) ) + 
  xlab( 'filename') + 
  ylab( 'Discrimination parameter')


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",  "#0072B2", "#F0E442", "#D55E00", "#CC79A7")

id_int %>%
  mutate( id_short = substr( id, 1 , 8)) %>% 
  mutate( id_ordered = factor( id_short, levels = unique(id_short[order(Estimate)]), ordered = T)) %>% 
  left_join( train %>% distinct(id, user_region ), by = 'id') %>% 
  ggplot(aes( x = id_ordered,  y = Estimate, ymin = Q2.5, ymax = Q97.5, color = user_region)) +
  geom_point( size = 3, alpha = 0.7) + 
  geom_errorbar(alpha = 0.7) + 
  coord_flip() + 
  scale_color_manual(values = cbPalette) + 
  ylab( 'Identification ability') + 
  xlab( 'Subject ID')


key_int %>% 
  group_by( key) %>% 
  transmute_at( .vars = c('Estimate', 'Est.Error', 'Q2.5', 'Q97.5'), .funs = function(x) -1*x )   %>% 
  ungroup() %>%
  mutate( key_f = factor( key, levels = key[ order( Estimate) ], ordered = T ))  %>%
  left_join(train %>% distinct( key, key_family ), by = 'key') %>% 
  ggplot( aes( x = key_f, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = key_family)) + 
  geom_point()  + 
  geom_errorbar() + 
  coord_flip() + 
  scale_color_brewer(type = 'seq', palette = 'RdYlBu', name  = 'Family') + 
  xlab( 'Species') +
  ylab( 'Difficulty') + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave( filename = 'figures/Taxa_difficulty.png', width = pw, height = ph)



key_int %>% 
  group_by( key ) %>% 
  transmute_at( .vars = c('Estimate', 'Est.Error', 'Q2.5', 'Q97.5'), .funs = function(x) -1*x ) %>% 
  ungroup() %>% 
  left_join( key_disc, by = 'key') %>% 
  ggplot( aes( x= Estimate.x, y= Estimate.y)) +geom_point() + 
  ggrepel::geom_text_repel(aes( label = key))

