rm(list = ls() )
library(tidyverse)
library(brms)
load( 'output/training_testing_data.rda')
load( 'output/fit_2pl.null.rda')

rm(fit_2pl.null)
rm(test)

# make covariate combos and fit all 

my_covs <- expand.grid( key_family = c("key_family", ""), 
                        photo_region = c("photo_region", ""), key_mivs = c("key_mivs", ""), taxa_repeat = c("taxa_repeat", ""), home_region = c('home_region', "")) %>%
  data.frame() %>% 
  filter( ! row_number() == 32 )

models <- 
  my_covs %>% 
  arrange( key_family, photo_region, home_region, key_mivs, taxa_repeat) %>% 
  filter( key_family == 'key_family', photo_region == 'photo_region') %>% 
  mutate( form = paste( key_family, photo_region, key_mivs, taxa_repeat, home_region, sep = '+')) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+", replacement = "+")) %>% 
  mutate( form = str_replace_all(form, pattern = "[\\+]+$", replacement = "")) %>% 
  mutate( form = paste0( "score ~ ", form ) ) %>% 
  mutate( re_form = '(1 |i| item) + (taxa_repeat | id)' ) %>% 
  mutate( re_form = ifelse( taxa_repeat == '', '(1 |i| item) + (1 | id)', re_form ))

# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Sampling parms: 
my_iter <- 4000 
my_cores <- 4 
my_thin <- 5 

# Ordered response model "2pl" 
# With item discrimination parameter
# No guessing parameter 

prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

for( i in 1:nrow( models )) { 
  temp_form <- as.formula( paste0( models$form[i], ' + ', models$re_form[i] ))
  
  temp_bf <- bf( temp_form, disc ~ 1 + (1 |i| item) )
  print( paste0( "working on model # ", i))
  print( temp_bf)
  temp_fit <- brm(
    formula = temp_bf,
    data = train,
    family = brmsfamily("cumulative", "logit"),
    prior = prior_2pl.full, cores = my_cores, iter = my_iter, thin = my_thin) # limit iterations for testing 
  
  outfile <- paste0( 'output/fit_2pl_model_', i, '.rda')
  
  save( temp_fit, file = outfile)
  rm( temp_fit )
}
