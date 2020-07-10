rm(list = ls() )
library(brms)
library(tidyverse)
load('output/training_testing_data.rda')
load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

ns <- 500  # number of posterior samples
pw <- 8 # image width 
ph <- 8 # image height 

# Get posterior samples from random effects: 
qs <- c(0.5, 0.025, 0.975) # quantiles to save 

disc_info <- 
  m3$data %>% 
  select( key, item )  %>% 
  bind_cols(
    posterior_linpred(m3, dpar = 'disc', nsamples = ns) %>% 
      exp() %>% 
      apply( 2, quantile , qs ) %>%
      t() %>% 
      data.frame()
  ) %>% 
  distinct() %>% 
  arrange( key, item ) %>% 
  group_by( key )  %>%
  mutate( avg_disc = mean(X50.))

disc_info$key_f <- factor( disc_info$key , levels = unique( disc_info$key[ order( disc_info$avg_disc)]), ordered = T)

disc_info %>%
  ggplot( aes( x = key_f, y = X50.)) + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6))  + 
  ggsave(filename = 'figures/discrimination_by_taxa_points.png', height = ph, width = pw )

disc_info %>%
  ggplot( aes( x = key_f, y = X50.)) + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6))
  
disc_ranks <- 
  disc_info %>% 
  ungroup() %>% 
  mutate( disc_rank = cut( X50., breaks = quantile( X50., c(0, 0.05, 0.95, 1)), include.lowest = T, labels = c('Low discrimination taxa','Med. disc.', 'High discrimination taxa') ) ) %>%
  filter( disc_rank != 'Med. disc.') 

random_fx <- ranef( m3)
id_int <- random_fx$id[, , 1] %>% data.frame() %>% mutate( id = rownames(.))

id_int <- 
  id_int %>% 
  arrange(Estimate) %>%
  mutate( id_q = factor( cut(Estimate, breaks = quantile(Estimate, c(0, 0.05, 0.95, 1)), include.lowest = T), labels = c('Low', 'Medium', 'High')) )

train %>% 
  left_join(disc_ranks, by = c('key', 'item')) %>%
  left_join(id_int, by = 'id') %>%
  filter( !is.na(disc_rank)) %>% 
  filter( id_q != 'Medium') %>% 
  rename( 'User knowledge' = id_q) %>% 
  ggplot( aes( x = score, group = `User knowledge`, fill = `User knowledge`)) + 
  geom_histogram(stat = 'count', position = 'dodge') + 
  facet_wrap( ~ disc_rank)  


# Extract discrimination for each taxa 
disc_Intercept <- posterior_samples(m3, pars = 'b_disc_Intercept')

key_disc <- 
  posterior_samples(m3, pars = 'r_key__disc') %>% 
  as_tibble() %>% 
  gather( key, disc, starts_with('r_key')) %>% 
  mutate( species = str_extract(key, pattern = '[A-Z][a-z\\.]+')) %>%
  mutate( species = str_replace(species, '\\.', ' '))

key_disc <- cbind( key_disc, disc_int = disc_Intercept[,1] )

key_disc_summary <- 
  key_disc %>% 
  mutate( disc_bt = exp( disc + disc_int )) %>% 
  group_by( species ) %>% 
  summarise( X50. = quantile(disc_bt, qs[1]), 
             X2.5. = quantile( disc_bt, qs[2]), 
             X97.5. = quantile( disc_bt, qs[3]))

key_disc_summary %>% 
  ungroup() %>% 
  mutate( species_ranked = factor( species, levels = unique( species[ order( X50.) ] ), ordered = T)) %>% 
  ggplot( aes( x = species_ranked, y = X50., ymin = X2.5., ymax = X97.5.)) + 
  geom_errorbar(width = 0.4, color = 'blue') + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Discrimination parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave(filename = 'figures/discrimination_by_taxa.png', height = ph, width = pw )

#

random_fx$id

item_diff <- random_fx$item[ , , 1] * (-1) # convert to negative to get difficulty 
key_diff <- random_fx$key[, , 1]*(-1) 

key_diff <- 
  key_diff %>% 
  data.frame( ) %>% 
  mutate( species = row.names(.)) %>% 
  mutate( key_f = factor( species, levels = unique(species)[order(Estimate, decreasing = T)])) 
  



key_diff %>% 
  ggplot( aes( x = key_f , y = Estimate , ymin = Q2.5, ymax = Q97.5)) + 
  geom_errorbar(width = 0.4, color = 'blue') + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Difficulty') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) + 
  ggsave(filename = 'figures/difficulty_by_taxa.png', height = ph, width = pw )

key_diff %>% 
  ggplot( aes( x = key_f , y = Estimate , ymin = Q2.5, ymax = Q97.5)) + 
  geom_errorbar(width = 0.4, color = 'blue') + 
  geom_point() + 
  xlab( 'Species' ) + 
  ylab( 'Difficulty') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) 
  

filename_key <- train %>% ungroup() %>% distinct(key, filename, difficulty)

item_samples <- 
  posterior_samples(m3, par = c('r_item\\[')) %>% 
  gather( item, Intercept, starts_with('r')) %>% 
  mutate( filename = str_extract(item, '[0-9.]+.jpg' )) %>% 
  left_join(filename_key, by = 'filename') %>% 
  select( - item ) %>% 
  group_by( filename ) %>%
  mutate( iteration = row_number()) %>% 
  ungroup()

key_samples <- 
  posterior_samples(m3, par = c('r_key\\[')) %>% 
  gather( key, Intercept, starts_with('r')) %>%
  mutate( key = str_extract( key , '[A-Z][a-z.]+')) %>%
  mutate( key = str_replace(key, '\\.', ' ')) %>% 
  group_by( key ) %>% 
  mutate( iteration = row_number()) %>% 
  ungroup()

cols <- scales::hue_pal()(2)

item_samples %>% 
  left_join(key_samples, by = c('key', 'iteration')) %>%
  mutate( Intercept = -1*(Intercept.x + Intercept.y)) %>% 
  select( key, filename, difficulty, iteration, Intercept) %>% 
  group_by( key, filename, difficulty ) %>%
  summarise( Intercept = quantile( Intercept, qs[1]), Q2.5 = quantile(Intercept, qs[2]), Q97.5 = quantile(Intercept, qs[3])) %>% 
  left_join(key_diff, by = c('key'='species')) %>%
  ggplot( aes( x = key_f, y = Intercept, color = difficulty, ymin = Q2.5.y, ymax = Q97.5.y )) + 
  geom_point() + 
  scale_color_manual(values = rev(cols), name = 'A priori image\ndifficulty class') + 
  xlab( 'Species' ) + 
  ylab( 'Item Difficulty Parameter') + 
  coord_flip() + 
  theme(axis.text.y = element_text(face = 'italic', size = 6)) +
  ggsave(filename = 'figures/image_difficulty_vs_estimated_difficulty.png', height = ph, width = pw )

item_samples %>% 
  mutate( Estimate = -1*Intercept ) %>% 
  group_by( filename, difficulty) %>% 
  summarise( Q50 = quantile(Estimate, qs[1])) %>%
  ggplot(aes( x = Q50, y = difficulty,  fill = difficulty )) + 
  ggridges::geom_density_ridges(alpha = 0.7) + 
  xlab( 'Item Difficulty Parameter') + 
  ylab( '') +
  scale_fill_manual(values = rev(cols), name = 'A priori\ndifficulty class') +
  scale_color_manual(values = rev(cols), name = 'A priori image\ndifficulty class') + 
  ggsave(filename = 'figures/item_difficulty_distribution_by_difficulty_class.png', height = ph, width = pw )

# actual data plot 
train %>% 
  group_by( difficulty, score ) %>%
  summarise( n = n() ) %>%
  group_by(difficulty ) %>% 
  mutate( prop = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot( aes( x = score, y = prop, fill = difficulty)) + 
  geom_bar(stat = 'identity', position = position_dodge()) + 
  scale_fill_manual(values = rev(cols), name = "A priori image\ndifficulty class") + 
  ylab("Proportion of responses") + 
  xlab("Identification Accuracy Score")
# 

# --------------------------- interpretation stuff 
library(boot)

Intercepts <- posterior_summary(m3, pars = 'b_Intercept')
id_int <- posterior_summary(m3, pars = 'r_id')
item_difficulty <- posterior_summary(m3, pars = 'r_item')
key_difficulty <- posterior_summary(m3, pars = 'r_key')

par(mfrow = c(1,2))
x <- seq( -10, 10, length.out = 100)
disc <- 0.8
plot(x , inv.logit( disc*(Intercepts[1,1] - x)), type = 'l', ylab = 'Probability', xlab = 'Linear predictor')
points(x, inv.logit( disc*(Intercepts[2,1] - x)) - inv.logit( disc*(Intercepts[1,1] - id_int[1,1])), type = 'l', col = 'blue')
points(x , inv.logit(disc*(Intercepts[3,1] - x)) - inv.logit( disc*(Intercepts[2,1] - id_int[1,1])), type = 'l', col = 'green')
points(x , 1 - inv.logit(disc*(Intercepts[3,1] - x)), type = 'l', col = 'red')
abline(v = id_int[1,1])
abline(h = inv.logit( disc*(Intercepts[1,1] - id_int[1, 1])), lty = 2)
abline(h = inv.logit( disc*(Intercepts[2,1] - id_int[1,1])) - inv.logit(disc*(Intercepts[1,1] - id_int[1,1])), col = 'blue', lty = 2)
abline(h = inv.logit( disc*(Intercepts[3,1] - id_int[1, 1])) - inv.logit( disc*(Intercepts[2,1] - id_int[1, 1])), col = 'green', lty = 2)
abline(h = 1 - inv.logit( disc*(Intercepts[3,1] - id_int[1,1])), col = 'red', lty = 2)
legend("topright", title = 'Score', legend = c(0, 1, 2, 3), col = c('black', 'blue', 'green', 'red'), lty = 1)
title(main = 'low discrimination')

# ------- 
disc <- 1.5
plot(x , inv.logit( disc*(Intercepts[1,1] - x)), type = 'l', ylab = 'Probability')
points(x, inv.logit( disc*(Intercepts[2,1] - x)) - inv.logit( disc*(Intercepts[1,1] - id_int[1,1])), type = 'l', col = 'blue')
points(x , inv.logit(disc*(Intercepts[3,1] - x)) - inv.logit( disc*(Intercepts[2,1] - id_int[1,1])), type = 'l', col = 'green')
points(x , 1 - inv.logit(disc*(Intercepts[3,1] - x)), type = 'l', col = 'red')
abline(v = id_int[1,1])
abline(h = inv.logit( disc*(Intercepts[1,1] - id_int[1, 1])), lty = 2)
abline(h = inv.logit( disc*(Intercepts[2,1] - id_int[1,1])) - inv.logit(disc*(Intercepts[1,1] - id_int[1,1])), col = 'blue', lty = 2)
abline(h = inv.logit( disc*(Intercepts[3,1] - id_int[1, 1])) - inv.logit( disc*(Intercepts[2,1] - id_int[1, 1])), col = 'green', lty = 2)
abline(h = 1 - inv.logit( disc*(Intercepts[3,1] - id_int[1,1])), col = 'red', lty = 2)
legend("topright", title = 'Score', legend = c(0, 1, 2, 3), col = c('black', 'blue', 'green', 'red'), lty = 1)
title(main = 'high discrimination')


