rm(list = ls())
library(tidyverse)
library(brms)  
library(ggridges)

load('output/training_testing_data.rda')
fit_files <- dir('output/', 'full_model_fit_', full.names = T)

# get formula

y_obs <- as.numeric( levels(test$score)[ test$score ] )

oos_accuracy <- loo_score <- oos_pred_df <- forms  <- list()
i <- 1
ns <- 100 
for( i in 1:length(fit_files)){ 
  load(fit_files[[i]])
  forms[[i]] <- temp_fit$formula
  loo_score[[i]] <- loo(temp_fit)
  
  p_score_re <- predict(temp_fit, newdata = test, nsamples = ns)
  y_pred_re <- p_score_re %*% c(0, 1, 2, 3) 

  # 
  p_score_id <- predict(temp_fit, newdata = test, re_formula = ~ (1|id), nsamples = ns) 
  y_pred_id <- p_score_id %*% c(0, 1, 2, 3)

  p_score_item <- predict(temp_fit, newdata = test, re_formula = ~ (1|item), nsamples = ns)
  y_pred_item <- p_score_item %*% c(0, 1, 2, 3)
  # 
  p_score_key <- predict(temp_fit, newdata = test, re_formula = ~ (1|key), nsamples = ns)
  y_pred_key <- p_score_key %*% c(0, 1, 2, 3)
  # 
  p_score_none <- predict(temp_fit, newdata = test, re_formula = NA , nsamples = ns)
  y_pred_none <- p_score_none %*% c(0, 1, 2, 3)
   
  oos_pred <- 
    data.frame(q_id = 1:length(y_obs),  y_obs = y_obs, obs_score = factor(y_obs), y_pred = y_pred_re, re = 'all')  
  
  oos_pred_none <-
    data.frame(q_id = 1:length(y_obs),  y_obs = y_obs, obs_score = factor(y_obs), y_pred = y_pred_none, re = 'none')
  # 
  oos_pred_id <-
    data.frame(q_id = 1:length(y_obs),  y_obs = y_obs, obs_score = factor(y_obs), y_pred = y_pred_id, re = 'id')
  # 
  oos_pred_item <-
    data.frame(q_id = 1:length(y_obs),  y_obs = y_obs, obs_score = factor(y_obs), y_pred = y_pred_item, re = 'item')
  # 
  oos_pred_key <-
    data.frame(q_id = 1:length(y_obs),  y_obs = y_obs, obs_score = factor(y_obs), y_pred = y_pred_key, re = 'key')
  # 
  oos_pred_df[[i]] <- 
    oos_pred %>%
    bind_rows(oos_pred_id) %>%
    bind_rows(oos_pred_key) %>%
    bind_rows(oos_pred_item) %>%
    bind_rows(oos_pred_none)
  
  oos_accuracy[[i]] <- 
    oos_pred_df[[i]] %>%
    ungroup() %>%
    group_by( re ) %>%
    mutate( sq_error = (y_pred - y_obs)^2 ) %>% 
    summarise( out_of_sample_RMSE = sqrt( mean( sq_error ) )) %>%
    mutate( model = i)

}


oos_accuracy <- do.call( rbind, mapply( x = oos_accuracy, y = 1:8, FUN = function(x, y) {x$model <- y; return(x) }, SIMPLIFY = F) ) 

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

oos_accuracy %>% arrange( model) %>% filter(re == 'all')

save(oos_pred_df, file = 'output/oos_scored.rda')
save(oos_accuracy, file = 'output/oos_accuracy.rda')
save(loo_score, file = 'output/loos_scores.rda')

loo_compare(loo_score[[1]], loo_score[[2]])

loos <- do.call( rbind, lapply( loo_score, function(x) x$estimate['looic', ]))
forms_simple <- do.call( rbind, lapply( forms, function(x) as.character( x$formula )[3] ))

loos_df <- loos %>% as.data.frame()  %>% mutate( model =  row_number())
loos_df$form <- forms_simple

loos_df[-c(5,7), ] %>% arrange( Estimate )

oos_accuracy %>%
  filter( re == 'all')
