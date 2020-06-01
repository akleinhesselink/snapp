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
                        key_mivs = c("key_mivs", ""), 
                        taxa_repeat = c("taxa_repeat", ""), 
                        home_region = c('home_region', ""), 
                        difficulty = c('difficulty', ""), 
                        ranef_include_key = c(T, F)) %>%
  data.frame() 


models <- 
  my_covs %>% 
  arrange( key_family, photo_region, home_region, key_mivs, taxa_repeat) %>% 
  filter( key_family == 'key_family', photo_region == 'photo_region', taxa_repeat == 'taxa_repeat', home_region == 'home_region', key_mivs == 'key_mivs', difficulty == 'difficulty') %>% 
  mutate( form = paste( key_family, photo_region, key_mivs, taxa_repeat, home_region, difficulty, sep = '+')) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+", replacement = "+")) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+$", replacement = "")) %>% 
  mutate( form = paste0( "score ~ ", form ) ) %>% 
  mutate( re_form = '(1 |i| item) + (taxa_repeat | id)' ) %>% 
  mutate( re_form = ifelse( taxa_repeat == '', '(1 |i| item) + (1 | id)', re_form )) %>% 
  mutate( re_form = ifelse( ranef_include_key , paste0(re_form , ' + (1 | key)'), re_form))


# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Sampling parms: 
my_iter <- 5000
my_cores <- 4 
my_thin <- 5 

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

my_priors  <- 
  list( 
  prior("constant(1)", class = "sd", group = "id", coef = "Intercept") +
  prior("normal(0, 1)", class = 'sd', group = 'id', coef = 'taxa_repeat') + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") 
  ,
  prior("constant(1)", class = "sd", group = "id", coef = "Intercept") +
  prior("normal(0, 1)", class = 'sd', group = 'id', coef = 'taxa_repeat') + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")  + 
  prior("normal(0, 3)", class = 'sd', group = 'key')) # item discrimination parameter with "disc" distribution 

names( my_priors ) <- c('no_key', 'with_key')


for( i in 1:nrow( models )) { 
  temp_form <- as.formula( paste0( models$form[i], ' + ', models$re_form[i] ))

  temp_bf <- bf( temp_form, disc ~ 1 + (1 |i| item) )
  print( paste0( "working on model # ", i))
  print( temp_bf)
  
  if( models$ranef_include_key[i] ) {  
    temp_prior <-  my_priors$with_key 
  }else{ 
    temp_prior <- my_priors$no_key 
  }
  
  temp_fit <- brm(
    formula = temp_bf,
    data = train,
    family = brmsfamily("cumulative", "logit"),
    prior = temp_prior, 
    cores = my_cores, 
    iter = my_iter, 
    thin = my_thin, seed = 1) 
  
  outfile <- paste0( 'output/full_model_fit_', i, '.rda')
  
  save( temp_fit, file = outfile)
  rm( temp_fit )
}
