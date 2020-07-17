rm(list = ls())
library(tidyverse)
library(brms)  

load('output/training_testing_data.rda')
fit_files <- dir('output/', 'full_model_fit_', full.names = T)

ns <- 200 

y_obs_in_sample <- as.numeric(train$score)
y_obs_oo_sample <- as.numeric(test$score)

fit_summary <- list()


for( i in 1:length(fit_files)){ 
  load(fit_files[[i]])
  fit_summary[[i]] <- data.frame( model = i, form = as.character(temp_fit$formula$formula[3]))
                                  
  y_hat_in_sample <- posterior_predict(temp_fit, newdata = train, nsamples = ns)
  y_hat_oo_sample <- posterior_predict(temp_fit, newdata = test, nsamples = ns)
  y_hat_oo_sample_no_re <- posterior_predict(temp_fit, newdata = test, nsamples = ns, re_formula = NA)
  
  R2_in_sample <- rstantools::bayes_R2(y_hat_in_sample, y = y_obs_in_sample ) %>% mean
  R2_oo_sample <- rstantools::bayes_R2(y_hat_oo_sample, y = y_obs_oo_sample ) %>% mean 
  R2_oo_sample_no_re <- rstantools::bayes_R2(y_hat_oo_sample_no_re, y = y_obs_oo_sample) %>% mean 
  
  MEA_in_sample <- apply( y_hat_in_sample, 1, function(x, y) mean(abs(x - y)), y = y_obs_in_sample ) %>% mean
  MEA_oo_sample <- apply( y_hat_oo_sample, 1, function(x, y) mean(abs(x - y)), y = y_obs_oo_sample ) %>% mean
  MEA_oo_sample_no_re <- apply( y_hat_oo_sample_no_re, 1, function(x, y) mean(abs(x - y)), y = y_obs_oo_sample ) %>% mean
  
  fit_summary[[i]]$MEA_in_sample <- MEA_in_sample 
  
  fit_summary[[i]]$MEA_oo_sample <- MEA_oo_sample 
  
  fit_summary[[i]]$MEA_oo_sample_no_re <- MEA_oo_sample_no_re
  
  fit_summary[[i]]$R2_in_sample <- R2_in_sample 
  fit_summary[[i]]$R2_oo_sample <- R2_oo_sample 
  fit_summary[[i]]$R2_oo_sample_no_re <- R2_oo_sample_no_re 
  
}

fit_summary <- do.call(bind_rows,  fit_summary) 

save(fit_summary , file = 'output/oos_accuracy.rda')

# oos_pred_df[[1]] %>%
#   ggplot( aes( x = y_pred, y = obs_score, fill = re)) +
#   annotate(x = 0:3, y = 1:4, geom = 'point') +
#   geom_density_ridges(rel_min_height = 0.005, alpha = 0.5) +
#   scale_y_discrete(expand = c(0.01, 0)) +
#   scale_x_continuous(expand = c(0.01, 0)) +
#   xlim(0, 3) +
#   xlab('Predicted Score') +
#   ylab('Observed Score') +
#   ggtitle('Out of sample scores, observed v. predicted') +
#   facet_wrap( ~ re )
