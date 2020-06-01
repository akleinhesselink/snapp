rm(list = ls())

library(tidyverse)
library(brms)
options(buildtools.check = function(action) TRUE )

load( 'output/training_testing_data.rda')
load( 'output/fit_2pl.null.rda')

rm(fit_2pl.null)
rm(test)

#  explicitly filter training data 

train <- 
  train %>%
  filter(!is.na( home_region ) ) %>% 
  group_by(id, filename)  %>% 
  arrange(id, filename, created_at) %>%  
  mutate( photo_repeat = row_number()) %>% 
  mutate( photo_repeat = ifelse( is.na(filename), NA, photo_repeat)) %>%  # remove photo repeats  
  filter( photo_repeat < 2 ) %>%
  arrange( id, created_at ) 


# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Sampling parms: 
my_iter <- 4000
my_cores <- 4 
my_thin <- 4 

# Model 3 redo with prior specification for taxa repeat slope 
# model 3.1 

prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id", coef = "Intercept") +
  prior("normal(0, 1)", class = "sd", group = "id", coef = "taxa_repeat") + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) + (taxa_repeat | id ) , 
               disc ~ 1 + (1 |i| item) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 


save(temp_fit, file = 'output/fit_form3.1.rda')

# Model 3 normal intercept prior 
# model 3.2 

prior_2pl.full <- 
  prior("normal(1, 1)", class = "sd", group = "id") +
  prior("normal(0, 1)", class = "sd", group = "id", coef = "taxa_repeat") + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) + (taxa_repeat | id ), 
               disc ~ 1 + (1 |i| item) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 

save(temp_fit, file = 'output/fit_form3.2.rda')

rm(temp_fit)

#### Model 3 with taxa random effects 
# model 3.3 
prior_2pl.full <- 
  prior("normal(1, 1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") # item discrimination parameter with "disc" distribution 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) + (1 | id ) , 
               disc ~ 1 + (1 |i| item) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 

save(temp_fit, file = 'output/fit_form3.3.rda')

rm( temp_fit )

#### Model three with item and key random effects 
## Model 3.4 
my_iter <- 2000 

prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id", coef = 'Intercept') +
  prior("normal(0, 1)", class = "sd", group = "id", coef = 'taxa_repeat') + 
  prior("normal(0, 3)", class = "sd", group = "key") + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) + (1 | key ) +  (taxa_repeat | id ) , 
               disc ~ 1 + (1 |i| item ) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 


save(temp_fit, file = 'output/fit_form3.4.rda')

# Model 3.5 taxa key random effect, NO taxa_repeat re 
# model 3.5 
prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "key") + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) + (1 | key ) +  (1 | id ) , 
               disc ~ 1 + (1 |i| item ) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 


save(temp_fit, file = 'output/fit_form3.5.rda')

### -------------------------------------------- # 
# Ordered response model "2pl" 
## model 3 original form 
# model 3.0 
prior_2pl.full <- 
  prior("constant(1)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc") 

temp_bf <- bf( score ~ photo_region + key_family + home_region + taxa_repeat + ( 1 | i | item ) +  (taxa_repeat | id ) , 
               disc ~ 1 + (1 |i| item ) )

temp_fit <- brm(
  formula = temp_bf,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.full, 
  cores = my_cores, 
  iter = my_iter, 
  thin = my_thin) # limit iterations for testing 

save(temp_fit, file = 'output/fit_form3.0.rda')
