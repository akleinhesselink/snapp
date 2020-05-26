rm(list = ls()) 
library(brms)
library(tidyverse)

load('data/snake_data.rda')
load('data/fit_1pl.null.rda')
load('data/fit_2pl.null.rda')
load('data/fit_2pl.full.rda')

summary( fit_1pl.null )
summary( fit_2pl.null)

plot(fit_1pl.null)
plot(fit_2pl.null)

fit1 <- add_criterion(fit_1pl.null, 'waic')
fit2 <- add_criterion(fit_2pl.null, 'waic')

loo_compare(fit1, fit2, criterion = 'waic')

# Model 2pl is preferred but there are some sampling issues. 

# Model with covariates 

fit_full <- add_criterion(fit_2pl.full, 'waic')

loo_compare(fit2, fit_full, criterion = 'waic')
loo(fit2)
loo(fit_full)

snake %>% 
  left_join(user_info, by = 'user_id') %>% 
  group_by( user_region ) %>% 
  summarise( n() , n_distinct(user_id))

df <- make_conditions(train_sample, vars = c('global_region'))

predict( fit_full, data.frame( global_region ='Africa', key_family = "Boidae", mivs = "n"), re_formula = NA)

out <- conditional_effects(fit_full, 'global_region', categorical = T, )

out$`global_region:cats__`$Accuracy_Level <- factor( out$`global_region:cats__`$effect2__, labels = c('incorrect', 'family', 'genus', 'binomial'))

out$`global_region:cats__` %>% 
  ggplot( aes( x = global_region, y = estimate__, ymin = lower__, ymax = upper__ , color = Accuracy_Level)) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_errorbar(position = 'dodge')
        

out <- conditional_effects(fit_full, 'key_family', categorical = T)

out$`key_family:cats__`$Accuracy_Level <- factor( out$`key_family:cats__`$effect2__, labels = c('incorrect', 'family', 'genus', 'binomial'))

out$`key_family:cats__` %>% 
  ggplot( aes( x = key_family, y = estimate__, ymin = lower__, ymax = upper__ , color = Accuracy_Level)) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_errorbar(position = 'dodge')
