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

pp_check_train <- 
  posterior_linpred(m3, newdata = train, nsamples = ns) %>% 
  apply( 2, quantile, qs) %>% t() %>% 
  data.frame() %>% 
  bind_cols(train ) 

pp_check_test <- 
  posterior_linpred(m3, newdata = test, nsamples = ns) %>% 
  apply( 2, quantile, qs) %>% t() %>% 
  data.frame() %>% 
  bind_cols(test) 

# aggregate to species
pp_check_by_species_train <- 
  pp_check_train %>%
  group_by( key ) %>% 
  summarise( y_hat = mean(X50.), avg_score = mean(as.numeric(score))) 

pp_check_by_species_test <- 
  pp_check_test %>%
  group_by( key ) %>% 
  summarise( y_hat = mean(X50.), avg_score = mean(as.numeric(score))) 

taxa_cor_train <- cor( pp_check_by_species_train$y_hat, pp_check_by_species_train$avg_score)
taxa_cor_test <- cor(pp_check_by_species_test$y_hat, pp_check_by_species_test$avg_score)

taxa_cor_train
taxa_cor_test

selected_taxa <- 
  pp_check_by_species_train %>%
  mutate( y_hat2 = scale(y_hat), avg_score2 = scale(avg_score) ) %>%
  mutate( deviation = y_hat2 - avg_score2 ) %>% 
  arrange( desc( abs(deviation) )) %>% 
  filter( row_number( ) < 6 )

pp_check_by_species_train %>% 
  ggplot( aes( x = y_hat, y = avg_score)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = selected_taxa, aes( label = key)) + 
  annotate(geom = 'text', x = 1,  y = 2.0, label = sprintf('r = %.2f', taxa_cor_train)) + 
  xlab('Linear predictor') + 
  ylab('Identification accuracy (score)') + 
  ggsave( 'figures/PP_check_species_train.png', height = ph, width = pw)

# Same plot for out of sample data 

selected_taxa <- 
  pp_check_by_species_test %>%
  mutate( dev = abs( scale(y_hat) - scale(avg_score))) %>% 
  arrange(desc(abs(dev))) %>%
  filter( row_number() < 6 )

pp_check_by_species_test %>% 
  ggplot( aes( x = y_hat, y = avg_score)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = selected_taxa, aes( label = key)) + 
  annotate(geom = 'text', x = 1,  y = 2.0, label = sprintf('r = %.2f', taxa_cor_test)) + 
  xlab('Linear predictor') + 
  ylab('Identification accuracy (score)') + 
  ggsave( 'figures/PP_check_species_test.png', height = ph, width = pw)


pp_check_train %>% 
  ggplot( aes( x = X50., y = as.numeric(score), fill = score)) + 
  ggridges::geom_density_ridges() + 
  scale_fill_discrete() + 
  ylab( "Linear predictor") + 
  ylab( "Distribution of scores")

# PP check for test takers 

pp_check_by_id_train <- 
  pp_check_train %>%
  group_by( id ) %>% 
  summarise( y_hat = mean(X50.), avg_score = mean(as.numeric(score))) 

pp_check_by_id_test <- 
  pp_check_test %>%
  group_by( id ) %>% 
  summarise( y_hat = mean(X50.), avg_score = mean(as.numeric(score))) 

id_cor_train <- cor( pp_check_by_id_train$y_hat, pp_check_by_id_train$avg_score)
id_cor_test <- cor(pp_check_by_id_test$y_hat, pp_check_by_id_test$avg_score)

pp_check_by_id_train %>% 
  ggplot( aes( x = y_hat, y = avg_score)) + 
  geom_point() + 
  annotate(geom = 'text', x = 1,  y = 2.0, label = sprintf('r = %.2f', id_cor_train)) + 
  xlab('Linear predictor') + 
  ylab('Average score by user') 

pp_check_by_id_test %>% 
  ggplot( aes( x = y_hat, y = avg_score)) + 
  geom_point() + 
  annotate(geom = 'text', x = 1,  y = 2.0, label = sprintf('r = %.2f', id_cor_test)) + 
  xlab('Linear predictor') + 
  ylab('Average score by user') 


