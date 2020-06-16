rm(list =ls())
library(brms)
library(tidyverse)
library(ggridges)

load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

load('output/oos_accuracy.rda')
load('output/oos_scored.rda')

oos_pred_df[[3]] %>%
  ggplot( aes( x = y_pred, y = obs_score, fill = re)) +
  annotate(x = 0:3, y = 1:4, geom = 'point') +
  geom_density_ridges(rel_min_height = 0.005, alpha = 0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  xlim(0, 3) +
  xlab('Predicted Score') +
  ylab('Observed Score') +
  ggtitle('Out of sample scores, observed v. predicted') +
  facet_wrap( ~ re )


oos_accuracy %>% filter( model == 3)
