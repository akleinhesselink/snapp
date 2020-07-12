rm(list = ls() )
library(brms)
library(tidyverse)
load('output/training_testing_data.rda')
load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

ns <- 1000  # number of posterior samples
pw <- 8 # image width 
ph <- 8 # image height 

# Get posterior samples from random effects: 
qs <- c(0.5, 0.025, 0.975) # quantiles to save 

# choose one photo_region per taxa 
key_info <- 
  train %>% 
  group_by( key ) %>% 
  filter( photo_region == photo_region[ which.max(n_distinct(item))]) %>% 
  ungroup() %>% 
  distinct(key_family, photo_region, key) %>% 
  mutate( taxa_repeat = 1 , home_region = T) 

key_diff <- posterior_linpred(m3, newdata = key_info, re.form = ~ (1|key), nsamples = ns )

key_diff <- 
  (-1*key_diff) %>% apply( 2, quantile, qs ) %>% t %>% 
  data.frame() %>%
  bind_cols( key_info )  %>% 
  mutate( key_f  = factor( key, levels = unique(key)[ order(X50.)], ordered = T))  

key_diff %>% 
  ggplot( aes( x = key_f, y = X50., ymin = X2.5., ymax = X97.5.)) + 
    geom_point() + 
    geom_errorbar(alpha = 0.7, width = 0.4) + 
    ylab( 'Difficulty' ) +
    xlab( 'Species') + 
    coord_flip() + 
    theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave('figures/Difficulty_by_species.png', width = pw, height = ph)
  
item_info <- 
  train %>% 
  group_by( key ) %>% 
  filter( photo_region == photo_region[ which.max(n_distinct(item))]) %>% 
  ungroup() %>% 
  distinct(key_family, photo_region, key, item, difficulty) %>% 
  mutate( taxa_repeat = 1 , home_region = T) 

item_diff <- posterior_linpred(m3, newdata = item_info, re.form = ~ (1|key) + (1|item), nsamples = ns )

item_diff <- 
  (-1*item_diff) %>% 
  apply( 2, quantile, qs ) %>% t %>% 
  data.frame() %>%
  bind_cols(item_info )  %>% 
  left_join(key_diff %>% distinct(key, key_f), by = 'key') 

cols <- scales::hue_pal()(2)

item_diff %>% 
  ggplot( aes( x = key_f, y = X50., color = difficulty)) + 
  geom_point() + 
  ylab( 'Difficulty' ) +
  xlab( 'Species') + 
  coord_flip() + 
  scale_color_manual(values = rev(cols), name = 'Image\ndifficulty class') + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave('figures/Difficulty_by_species_and_image.png', width = pw, height = ph)



