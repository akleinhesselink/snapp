rm(list = ls() ) 
options(buildtools.check = function(action) TRUE )

library(brms)
library(tidyverse)

load( 'output/training_testing_data.rda')

my_cores <- 4
my_iter <- 6000
my_thin <- 10

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

prior_2pl.null_key_item <- 
  prior("constant(1)", class = "sd", group = "id" ) +
  prior("normal(0, 1)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "key") + 
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")  + 
  prior("normal(0, 1)", class = "sd", group = "key", dpar = "disc") # item discrimination parameter with "disc" distribution 


form_2pl.null_key_item <- bf( score ~ 1 + key_family + taxa_repeat + (1 |i| item) + (1 | id) + (1 |k | key),
                          disc ~ 1 + (1 | i | item) + (1 | k | key ) )



fit_null_key_item <- brm(
  formula = form_2pl.null_key_item,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.null_key_item, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 

save( fit_null_key_item,  file = 'output/fit_2pl.null_key_item.rda')

rm(fit_null_key_item)



