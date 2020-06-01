rm(list = ls()) 
library(brms)
library(tidyverse)

load('output/fit_2pl_model_3.rda')
fit3 <- temp_fit
load( 'output/fit_form3_weaker_prior.rda')
fit3 <- fit1

summary( fit3 ) 

out <- conditional_effects(fit3, 'home_region', categorical = T)

out$`photo_region:cats__`$Accuracy_Level <- factor( out$`photo_region:cats__`$effect2__, labels = c('incorrect', 'family', 'genus', 'binomial'))

out$`photo_region:cats__` %>% 
  ggplot( aes( x = photo_region, y = estimate__, ymin = lower__, ymax = upper__ , color = Accuracy_Level)) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_errorbar(position = 'dodge')

out <- conditional_effects(fit3, 'key_family', categorical = T)

out$`key_family:cats__`$Accuracy_Level <- factor( out$`key_family:cats__`$effect2__, labels = c('incorrect', 'family', 'genus', 'binomial'))

out$`key_family:cats__` %>% 
  ggplot( aes( x = key_family, y = estimate__, ymin = lower__, ymax = upper__ , color = Accuracy_Level)) + 
  geom_point(position = position_dodge(width = 0.9)) + 
  geom_errorbar(position = 'dodge')

out2 <- conditional_effects(fit3, 'home_region', categorical = T)

out2

out3 <- conditional_effects(fit3, 'taxa_repeat', categorical = T)

out3
