rm(list = ls() ) 
library(brms)
library(tidyverse)
load( 'output/training_testing_data.rda')

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

prior_2pl.null <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

form_2pl.null <- bf( score ~ 1 + (1 |i| item) + (1 | id),
                        disc ~ 1 + (1 |i| item) 
                        )

fit_2pl.null <- brm(
  formula = form_2pl.null,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.null, cores = my_cores, iter = my_iter, thin = my_thin) # limit iterations for testing 

save( fit_2pl.null,  file = 'output/fit_2pl.null.rda')


