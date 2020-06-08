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

responses <- 
  responses %>%
  filter( !is.na(photo_region )) 

# Setting up model: 
#   Based on Burkner (2020), I believe the best way to analyze the tests
#   is with a graded response ordinal model.  We can try to fit a model 
#   with or without a guessing parameter.  This model can have user effects
#   and question covariates ( snake region or family). 

# Relabeling user and question
#   To make interpretation neater I recode user ID and question (filename) as integers 
#   We will use the standard names "id" and "item" for the user and question respectively.  This 
#   matches the terminology used in IRT. 

responses$id <- responses$id
responses$item <- responses$filename
responses$score <- factor(responses$score, levels = c(0, 1, 2, 3), ordered = T)

# Check for duplicate images shown to same user 
responses %>%
  group_by( id, filename) %>% summarise( n = n()) %>% filter( n > 1  )

responses <-  
  responses %>%
  group_by( id ) %>% 
  mutate( items_per_user = n() )

# Filter to users with at least "min_q" questions answered 
min_q <- 80 # n question limit 

responses <- 
  responses %>% 
  filter( items_per_user > min_q)

# Split to testing and training 
# model development.  (reduce wait time for sampling). 
set.seed(1)

responses$fold <- loo::kfold_split_grouped(2, x = responses$id )

train <- 
  responses %>% 
  filter(fold %in% c(1))

test <-
  responses %>% 
  filter(fold == 2)


nrow( train )
n_distinct(train$id)
n_distinct(train$item)

nrow( test )
n_distinct(test$id)
n_distinct(test$item)

save( train, test, file = 'output/training_testing_data.rda')
