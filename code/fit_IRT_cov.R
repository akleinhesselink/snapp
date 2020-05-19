rm(list = ls() )
library(tidyverse)
library(brms)
load( 'Data/snake_data.rda')
load( 'Data/fit_2pl.null.rda')

# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Sampling parms: 
my_iter <- 1000 
my_cores <- 4 
my_thin <- 2 

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

form_2pl.full <- bf( score ~ key_family + global_region + mivs + (1 |i| item) + (1 | id),
                     disc ~ 1 + (1 |i| item) 
)


fit_2pl.full <- brm(
  formula = form_2pl.full,
  data = train_sample,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, cores = my_cores, iter = my_iter) # limit iterations for testing 


library(lme4)

train_sample$score2 <-  as.numeric( train_sample$score ) - 1
mer_full <- lmer( score2 ~ key_family + global_region + mivs + (1|item) + (1|id), data = train_sample)

emmeans::emmeans(mer_full, ~ key_family )  %>% plot 
emmeans::emmeans(mer_full, ~ global_region) %>% plot 

save( fit_2pl.full, train_sample, test_sample, file = 'data/fit_2pl.full.rda')

