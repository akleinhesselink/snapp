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

# drop file numbers above "1000";  These are not scored
responses <- 
  responses %>%
  filter( str_extract(filename, '\\d+') < 1001 ) %>% 
  arrange( id, created_at, filename, response ) 

# Calculate total number of records dropped  
( raw_responses %>% nrow() )  - (responses %>% nrow() ) 

rm(raw_responses)

photo_key <- read_csv('raw_data/filename key.csv') %>% select( - X1 )

# ODD photos are easy, even photos are hard
photo_key <- 
  photo_key %>% 
  arrange( filename) %>% 
  mutate( difficulty = row_number() %% 2 == 0 ) %>% 
  mutate( difficulty = factor( difficulty, labels = c('easy', "hard")))
  

user_info <- read_csv('raw_data/snapp_users_noemail.csv')     # list of user information 
taxa_info <- read_csv('raw_data/species incl mivs for Andy out.csv') %>% select( - X1 )  # list of taxonomic information for taxa in photos and taxa not in photos 

# Make table of taxonomic information for all species, genera and family 
# Fill in information for photo keys and user responses 

binomial_info <- taxa_info %>% select(binomial, genus, family) %>% distinct()
genus_info    <- taxa_info %>% select( genus, family) %>% distinct()
family_info   <- taxa_info %>% select( family ) %>% distinct()

all_taxa_info <- 
  taxa_info %>% 
  select(family, genus, binomial, mivs, inCSchallenge ) %>% 
  gather(taxonomic_level, taxa, c(family, genus, binomial)) %>% 
  select( taxa, taxonomic_level, inCSchallenge, mivs ) %>% 
  group_by( taxa, taxonomic_level) %>% 
  mutate(   mivs = ifelse( taxonomic_level != 'binomial' & n_distinct(mivs) > 1, 'ambiguous', mivs),  # classify mivs at higher taxonomic levels
            inCSchallenge = ifelse ( any( inCSchallenge == 1), 1, 0)) %>% 
  distinct() 

# Make tables with binomial, genus and family level information. 
# Binomial has genus, family and MIVs status 
# Genus has family and MIVS status 
# Family has only MIVS status 

binomial_info <- 
  binomial_info %>% 
  left_join( all_taxa_info, by = c('binomial' = 'taxa' )) %>% 
  rename( taxa = binomial ) 

genus_info <- 
  genus_info %>% 
  left_join( all_taxa_info, by = c('genus' = 'taxa'))  %>% 
  mutate( taxa = genus ) 

family_info <- 
  family_info %>% 
  left_join( all_taxa_info, by = c('family' = 'taxa')) %>% 
  mutate( taxa = family, genus = NA) 

all_taxa_info <- 
  bind_rows( binomial_info, genus_info, family_info)

# Make photo key with taxa information 

photo_key <- 
  photo_key %>% 
  rename( 'key' = correct.binomial) %>% 
  rename( 'photo_region' = global_region) %>%
  left_join(all_taxa_info, by = c('key' = 'taxa')) %>% 
  rename( 'key_genus' = genus, 
          'key_family' = family, 
          'key_mivs' = mivs, 
          'key_taxonomic_level' = taxonomic_level )  %>% 
  distinct()

# Make response table with taxa info
response_taxa <- 
  all_taxa_info %>% 
  select( - inCSchallenge ) %>% 
  rename( 'response' = taxa, 
          'response_genus' = genus, 
          'response_family' = family, 
          'response_taxonomic_level' = taxonomic_level, 
          'response_mivs' = mivs )  %>% 
  distinct()

# Make table with all user information 
user_info <-
  user_info %>% 
  rename( "user_region" = region) %>% 
  mutate( user_region = str_to_title( str_replace(user_region, '-', ' ' )) ) %>% 
  mutate( user_region = ifelse( user_region == 'Australasia Oceania' , 'Australasia/Oceania', user_region)) %>% # format to match taxa region
  distinct() 

all_ids <- unique(responses$id)

# Total number of distinct users 
length(all_ids) 

user_info <- 
  tibble( id = all_ids )  %>% 
  left_join(user_info, by = 'id' ) %>% 
  distinct() %>% 
  arrange( id ) 

user_info %>%
  group_by( user_region ) %>% 
  summarise( n_users =  n() )

# check for duplicates: 
user_info %>% 
  group_by( id ) %>% 
  filter( n() > 1)

### Join filename and response information on taxa ---------------------- # 
responses <- 
  responses %>% 
  left_join(photo_key, by = 'filename') %>%
  left_join(response_taxa, by = 'response')

# calculate taxa repeats 
responses <- 
  responses %>% 
  group_by(id, key ) %>% 
  arrange( id, key, created_at ) %>%
  mutate( taxa_repeat = row_number() )

# Show taxa that show up more than 10 times 
responses %>% 
  group_by( key ) %>% 
  summarise( rep =  max( taxa_repeat)) %>% 
  filter( rep > 10 )

## Join user info for user region  
## Calculate "home_region" field when user region is provided 
responses %>%nrow()

responses <- 
  responses %>% 
  ungroup() %>% 
  left_join( user_info, by = 'id') %>%
  mutate( skip = is.na(response)) %>% 
  mutate( home_region = user_region == photo_region ) %>%             
  mutate( home_region = ifelse( is.na(photo_region) | is.na(user_region), NA, home_region)) 

## Calculate user scores: 
## Match user responses to taxa information  
## Score user response:  Correct species == 3, correct genus = 2, correct family = 1, incorrect or skip = 0 
responses <- 
  responses %>% 
  rowwise() %>%
  mutate( score = replace_na(key == response, 0 ) + 
            replace_na( key_genus == response_genus, 0) + 
            replace_na( key_family == response_family, 0))  

# Show any repeats that exist 
responses %>% 
  ungroup() %>%
  group_by( id, filename) %>% 
  filter( n() > 1 ) 

responses %>% 
  write_csv('output/cleaned_response_data.csv')

