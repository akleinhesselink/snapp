rm(list = ls())
library(tidyverse)
library(brms)

responses <- read_csv( 'output/cleaned_response_data.csv')

# User responses are graded as: 
#   0: family incorrect or skipped
#   1: fammily correct 
#   2: genus correct 
#   3: binomial correct 

# Show number "inCSchallenge"
responses %>% 
  group_by( inCSchallenge) %>% 
  summarise( n() )

# Filter out questions where "inCSchallenge" = 0 (FALSE)
responses <- 
  responses %>% 
  filter( inCSchallenge == 1)

# Exclude users with no region: 
responses %>% 
  group_by( user_region ) %>% 
  summarise(n())

responses <- 
  responses %>% 
  filter( !is.na(user_region)) 

# Setting up model: 
#   Based on Burkner (2020), I believe the best way to analyze the tests
#   is with a graded response ordinal model.  We can try to fit a model 
#   with or without a guessing parameter.  This model can have user effects
#   and question covariates ( snake region or family). 

# Relabeling user and question
#   To make interpretation neater I recode user ID and question (filename) as integers 
#   We will use the standard names "id" and "item" for the user and question respectively.  This 
#   matches the terminology used in IRT. 

responses$id <- as.numeric( factor( responses$id ) )
responses$item <- as.numeric( factor( responses$filename ) )
responses$score <- factor(responses$score, levels = c(0, 1, 2, 3), ordered = T)

# Subset to smaller testing data set for 
# model development.  (reduce wait time for sampling). 
set.seed(1)

responses$fold <- loo::kfold_split_grouped(3, x = responses$id )

train <- 
  responses %>% 
  filter(fold %in% c(1,2))

test <-
  responses %>% 
  filter(fold == 3)


# Filter to users with at least "min_q" questions answered 
# For model development only, 
# Done to speed model fitting 

min_q <- 100 # n question limit 

train <- 
  train %>% 
  group_by( id ) %>%
  mutate( items_per_user = n()) %>% 
  group_by( item) %>% 
  mutate( users_per_item = n() ) %>% 
  filter( items_per_user > min_q  ) # user answered question limit 

test <- 
  test %>% 
  group_by( id ) %>%
  mutate( items_per_user = n()) %>% 
  group_by( item) %>% 
  mutate( users_per_item = n() ) %>% 
  filter( items_per_user > min_q  ) # include users that answered more than 20 questions

nrow( train )
n_distinct(train$id)
n_distinct(train$item)

nrow( test )
n_distinct(test$id)
n_distinct(test$item)

# Sampling parms: 
my_iter <- 3000 
my_cores <- 4 
my_thin <- 5 

# Ordered response model "1pl" 
# No item discrimination parameter
# No guessing parameter 

prior_1pl.null <-
  prior("normal(0, 3)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

form_1pl.null <- bf(score ~ 1 + (1 | item) + (1 | id ))

fit_1pl.null <- brm(
  formula = form_1pl.null,
  data = train,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_1pl.null, cores = my_cores, iter = my_iter, thin = my_thin) # limit iterations for testing 

save( train, test, file = 'output/training_testing_data.rda')

save( fit_1pl.null, file = 'output/fit_1pl.null.rda')

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

plot(fit_2pl.null)
