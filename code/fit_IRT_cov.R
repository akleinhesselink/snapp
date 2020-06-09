rm(list = ls() )
library(tidyverse)
library(brms)
options(buildtools.check = function(action) TRUE )

load( 'output/training_testing_data.rda')
rm(test)

# make covariate combos and fit all 
# make combinations of covariates to fit 
my_covs <- expand.grid( key_family = c("key_family", ""), 
                        photo_region = c("photo_region", ""), 
                        taxa_repeat = c("taxa_repeat", ""), 
                        home_region = c('home_region', ""), 
                        difficulty = c('difficulty', ""), 
                        region_by_home = c("photo_region:home_region", "")) %>%
  data.frame() 


models <- 
  my_covs %>% 
  arrange( key_family, photo_region, home_region, key_mivs, taxa_repeat) %>% 
  filter( key_family == 'key_family', photo_region == 'photo_region', taxa_repeat == 'taxa_repeat') %>% 
  mutate( form = paste( key_family, photo_region, key_mivs, taxa_repeat, home_region, difficulty, sep = '+')) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+", replacement = "+")) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+$", replacement = "")) %>% 
  mutate( form = paste0( "score ~ ", form ) ) 

prior_2pl.null_key_item <- 
  prior("constant(1)", class = "sd", group = "id", coef = "Intercept" ) +
  prior("normal(0, 1)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "key") + 
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")  + 
  prior("normal(0, 1)", class = "sd", group = "key", dpar = "disc") # item discrimination parameter with "disc" distribution 


models$re_form <- "(1 |i| item) + (1 | id) + (1 |k | key)"

# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Sampling parms: 
my_iter <- 2000
my_cores <- 4 
my_thin <- 1 

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

my_prior <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 1)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "key") + 
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")  + 
  prior("normal(0, 1)", class = "sd", group = "key", dpar = "disc") # item discrimination parameter with "disc" distribution 

for( i in 1:nrow( models )) { 
  temp_form <- as.formula( paste0( models$form[i], ' + ', models$re_form[i] ))

  temp_bf <- bf( temp_form, disc ~ 1 + (1 | i | item) + (1 | k | key) )
  print( paste0( "working on model # ", i))
  print( temp_bf)
  
  temp_fit <- brm(
    formula = temp_bf,
    data = train,
    family = brmsfamily("cumulative", "logit"),
    prior = my_prior, 
    cores = my_cores, 
    iter = my_iter, 
    thin = my_thin, 
    seed = 1) 
  
  outfile <- paste0( 'output/full_model_fit_', i, '.rda')
  
  save( temp_fit, file = outfile)
  rm( temp_fit )
}
