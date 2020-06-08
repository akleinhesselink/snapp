rm(list = ls())
require( tidyverse)

raw_responses <- read_tsv('raw_data/snapp_responses.txt')  # list of responses 

## clean up and rename columns 
responses <- 
  raw_responses %>% 
  select( user_id, created_at, filename, `user_time(ms)`, answer) %>% 
  rename( 'response' = answer , 
          'id' = user_id, 
          'response_time' = `user_time(ms)`) %>% 
  mutate( created_at = as.POSIXct( strptime(created_at, format = '%m/%d/%Y %H:%M:%S', tz = 'UTC' ))) %>% 
  arrange( id, created_at )  %>% 
  distinct() 


## Check for duplicate records 
# Different response times for same exact filename user id, response and datestamp!! ?  
duplicate_rt <- 
  responses %>% 
  group_by( id, created_at, filename, response) %>%
  mutate( n= n_distinct(response_time)) %>% 
  arrange( desc( n)) %>% 
  filter( n > 1 )

# system appears to be logging multiple response times for the same question and user 
# these are only a few milliseconds off 
duplicate_rt %>%
  summarise( max( response_time) - min(response_time)) 

# We are not using response time so we will drop this and recalculate the distinct records 
responses <- 
  responses %>% 
  ungroup() %>% 
  select( id, created_at, filename, response) %>% 
  distinct() 

# there is one case where the same question at the same time got two different answers !!!? 
responses %>% group_by( id, created_at ) %>% mutate( n = n() ) %>% filter( n > 1 )

# We will take the one that is not NA 
responses <- 
  responses %>% 
  group_by( id, created_at ) %>% 
  mutate( repeated = row_number() ) %>% 
  filter( repeated == 1 ) %>% 
  select( - repeated )

# Many cases where the same exact file is shown to the same exact user within a short period of time!!!? 
responses %>%
  arrange( id, filename, created_at )  %>% 
  group_by( id, filename ) %>% 
  mutate( n_file_repeats = row_number()  )  %>%
  filter( n_distinct(n_file_repeats) > 1 ) 

# We will take the first instance that a file is shown to a user 
responses <- 
  responses %>%
  arrange( id, filename, created_at )  %>% 
  group_by( id, filename ) %>% 
  mutate( n_file_repeats = row_number()  )  %>%
  filter( n_file_repeats == 1 ) %>% 
  select( id, filename, created_at, response) 

# dropped 393 duplicated records of one kind or another 
( raw_responses %>% nrow() )  - (responses %>% nrow() ) 
