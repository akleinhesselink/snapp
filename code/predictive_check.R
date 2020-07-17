rm(list = ls() )
library(brms)
library(tidyverse)
library(bayesplot)

load('output/training_testing_data.rda')
load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

ns <- 500  # number of posterior samples
pw <- 8 # image width 
ph <- 8 # image height 

# Get posterior samples from random effects: 
qs <- c(0.5, 0.025, 0.975) # quantiles to save 

yhat.train <- posterior_predict(m3, newdata = train, nsamples = ns)
yhat.test  <- posterior_predict(m3, newdata = test, nsamples = ns)

y_test <- as.numeric( test$score ) # observed in testing data 
y_train <- as.numeric(train$score) # observed in training data 

pp_check_fam_in_samp <- ppc_bars_grouped(y_train, 
                                                    yrep = 
                                                      yhat.train, prob = 0.95, 
                                                    group = train$key_family, freq = F, 
                                                    fatten = 1, size = 0.5)

ggsave( pp_check_fam_in_samp, filename = 'figures/pp_check_family_in.png', height = 4, width = 6)


pp_check_fam_oo_samp <- ppc_bars_grouped(y_test, yrep = yhat.test, prob = 0.95, group = test$key_family, freq = F, 
                            fatten = 1, size = 0.5)

ggsave( pp_check_fam_oo_samp, filename = 'figures/pp_check_family_out.png', height = 4, width = 6)

pp_check_species_oo_samp <- ppc_bars_grouped(y_train, yrep = yhat.train, prob = 0.95, group = train$key, freq = F, 
                                                    fatten = 1, size = 0.5, facet_args = list( ncol = 5))

pp_check_species_oo_samp$data


pp_check_train <- 
  posterior_predict(m3, newdata = train, nsamples = ns) 

pp_check_by_sp <- split( pp_check_train %>% t() %>% data.frame(), f = train$key)

my_summarize <- function( x ) { 
  try( x %>%  
    apply( 2, function(y) table( y) / length(y)) %>% 
             apply( 1, function( z ) quantile( z, qs )) 
  )
}
pp_exp_by_sp <- lapply( pp_check_by_sp, my_summarize)
prop_observed_by_species <- lapply ( ( train %>% split( f = train$key  )), function( x ) table( as.numeric(x$score) )/length( x$score )  )

pp_exp_by_sp$`Acanthophis antarcticus`
prop_observed_by_species$`Acanthophis antarcticus`

barplot( unlist( pp_check_by_sp$`Acanthophis antarcticus` ), freq = F )
hist( as.numeric( train$score[train$key == 'Acanthophis antarcticus']))

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


