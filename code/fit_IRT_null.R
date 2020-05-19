rm(list = ls())
library(tidyverse)
library(brms)
load( 'Data/snake_data.rda')

# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

responses <- 
  snake %>% 
  left_join(user_info, by = 'user_id') %>% 
  mutate( not_skipped = 1 - skip , home_region = user_region == global_region ) %>% 
  mutate( score = not_skipped*( family_correct + genus_correct + binomial_correct)) %>% 
  select( user_id, user_region, home_region, user_time_sec, filename, mivs, key_family, global_region, not_skipped, family_correct, genus_correct, binomial_correct, score ) 

# Setting up model: 
#   Based on Burkner (2020), I believe the best way to analyze the tests
#   is with a graded response ordinal model.  We can try to fit a model 
#   with or without a guessing parameter.  This model can have user effects
#   and question covariates ( snake region or family). 

# Relabeling user and question
#   To make interpretation a little neater I will recode user ID and question (filename) as integers 
#   We will use the standard names "id" and "item" for the user and question respectively.  This 
#   matches the terminology used in IRT. 

responses$id <- as.numeric( factor( responses$user_id ) )
responses$item <- as.numeric( factor( responses$filename ) )
responses$score <- factor(responses$score, levels = c(0, 1, 2, 3), ordered = T)

# Subset to smaller testing data set for 
# model development.  (reduce wait time for sampling). 
set.seed(1)

responses$fold <- loo::kfold_split_grouped(5, x = responses$id )

train_sample <- 
  responses %>% 
  filter(fold == 1)

test_sample <-
  responses %>% 
  filter(fold %in% c(2:3))

# Filter to users with at least "min_q" questions answered 
# For model development only, 
# Done to speed model fitting 

flag_skipped <- 1
min_q <- 30 # n question limit 
min_u <- 20 # max n questions 

train_sample <- 
  train_sample %>% 
  filter( not_skipped %in% flag_skipped ) %>% 
  group_by( id ) %>%
  mutate( items_per_user = n()) %>% 
  group_by( item) %>% 
  mutate( users_per_item = n() ) %>% 
  filter( items_per_user > min_q  ) %>% # include users that answered more than 20 questions
  filter( users_per_item > min_u ) # include questions that at least 30 users answered 

test_sample <- 
  test_sample %>% 
  filter( not_skipped %in% flag_skipped ) %>% 
  group_by( id ) %>%
  mutate( items_per_user = n()) %>% 
  group_by( item) %>% 
  mutate( users_per_item = n() ) %>% 
  filter( items_per_user > min_q  ) %>% # include users that answered more than 20 questions
  filter( users_per_item > min_u ) # include questions that at least 30 users answered 

nrow( train_sample )
n_distinct(train_sample$id)
n_distinct(train_sample$item)

nrow( test_sample )
n_distinct(test_sample$id)
n_distinct(test_sample$item)

# Sampling parms: 
my_iter <- 1000 
my_cores <- 4 
my_thin <- 2 

# Ordered response model "1pl" 
# No item discrimination parameter
# No guessing parameter 

prior_1pl.null <-
  prior("normal(0, 3)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

form_1pl.null <- bf(score ~ 1 + (1 | item) + (1 | id ))

fit_1pl.null <- brm(
  formula = form_1pl.null,
  data = train_sample,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_1pl.null, cores = my_cores, iter = my_iter) # limit iterations for testing 

save( fit_1pl.null, train_sample, test_sample,  file = 'data/fit_1pl.null.rda')

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
  data = train_sample,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_2pl.null, cores = my_cores, iter = my_iter) # limit iterations for testing 

save( fit_2pl.null, train_sample, test_sample, file = 'data/fit_2pl.null.rda')

