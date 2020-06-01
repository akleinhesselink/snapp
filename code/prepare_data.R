rm(list = ls())
require( tidyverse)

responses_1 <- read_tsv('raw_data/snapp_responses.txt')  # list of responses 
responses_2  <- read_csv('raw_data/snapp_responses_out-corr-only100spp-toErol.csv') # list of responses with photo taxa corrected
responses_3  <- read_csv('raw_data/DT.sm100.csv')   # includes user region information 

user_info <- read_csv('raw_data/snapp_users_noemail.csv')     # list of user information 
taxa_info <- read_csv('raw_data/species incl mivs for Andy out.csv')  # list of taxonomic information for taxa in photos and taxa not in photos 

# Make key with correct ID for each unique photo 
photo_key <- 
  bind_rows( 
  responses_1 %>% 
    select( filename, binomial ) %>% 
    rename( 'key' = binomial ) %>% 
    mutate( dataset = 1, photo_region = NA), 
  responses_2 %>% 
    select(filename, species, global_region, difficulty) %>% 
    rename( 'key' = species, 'photo_region' = global_region) %>% 
    mutate( dataset = 2), 
  responses_3 %>% 
    select(filename, species, global_region, difficulty ) %>% 
    rename( 'key' = species, 'photo_region'  = global_region ) %>% 
    mutate( dataset = 3)
  ) %>%  
  distinct() %>% 
  group_by( filename ) %>%
  arrange( desc(dataset)) %>% 
  filter( row_number() == 1 )  %>% # keep corrected photo_key's from dataset 2 and 3. 
  distinct(filename, key, photo_region, difficulty ) %>% 
  arrange( filename )

# photo_key %>% View # show photo_key 

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

# Join photo key with taxa information 
photo_key <- 
  photo_key %>% 
  left_join(all_taxa_info, by = c('key' = 'taxa')) %>% 
  rename( 'key_genus' = genus, 
          'key_family' = family, 
          'key_mivs' = mivs, 
          'key_taxonomic_level' = taxonomic_level )  %>% 
  distinct()

# Make table to join to user responses 
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

all_ids <- unique( c(user_info$id, responses_1$user_id, responses_2$user_id, responses_3$user_id )) # get all unique user IDs

user_info <- 
  tibble( id = all_ids )  %>% 
  left_join(user_info, by = 'id' ) %>% 
  distinct() %>% 
  arrange( id ) 

user_info %>%
  group_by( user_region ) %>% 
  summarise( n() )

# check for duplicates: 
user_info %>% 
  group_by( id ) %>% 
  filter( n() > 1)


### Format all response information ---------------------- # 

# remove duplicates 
responses <- 
  responses_1 %>% 
  group_by( user_id, filename, created_at) %>% 
  mutate( n = n()) %>%
  arrange(desc(n), is.na(answer)) %>% 
  filter( row_number() == 1 ) %>% # take only the first record of each group
  select( -n ) %>% 
  ungroup() %>% 
  distinct() 

responses <- 
  responses %>% 
  rename( 'response' = answer , 
          'id' = user_id, 
          'response_time' = `user_time(ms)`) %>% 
  mutate( created_at = as.POSIXct( strptime(created_at, format = '%m/%d/%Y %H:%M:%S', tz = 'UTC' ))) %>% 
  select( id, filename, created_at, response_time, response) %>% 
  distinct() %>% 
  left_join(photo_key, by = 'filename') %>% 
  group_by(id, key)  %>% 
  arrange(id, key, created_at) %>%  
  mutate( taxa_repeat = row_number()) %>% 
  mutate( taxa_repeat = ifelse( is.na(key), NA, taxa_repeat)) %>%  # calculate taxa repeats 
  distinct() 

responses %>% filter( id == "b9c4207d-ccf3-42b5-b6ef-203559869d05") %>% View

## Join user info for user region  
## Calculate "home_region" field when user region is provided 

responses <- 
  responses %>% 
  ungroup() %>% 
  group_by( id ) %>% 
  mutate( user_start = min(created_at)) %>% 
  arrange( user_start, created_at) %>% 
  mutate( unique_question = row_number() ) %>% 
  left_join( user_info, by = 'id') %>%
  mutate( inCSchallenge = ifelse( is.na(inCSchallenge), 0, inCSchallenge)) %>% 
  mutate( skip = is.na(response)) %>% 
  mutate( home_region = user_region == photo_region ) %>%             
  mutate( home_region = ifelse( is.na(photo_region) | is.na(user_region), NA, home_region)) 


## Calculate user scores: 
## Match user responses to taxa information  
## Score user response:  Correct species == 3, correct genus = 2, correct family = 1, incorrect or skip = 0 

responses <- 
  responses %>%
  left_join(response_taxa, by = 'response') %>% 
  mutate( correct_binomial = (response == key), 
          correct_genus = (response_genus == key_genus), 
          correct_family = (response_family == key_family) ) %>%
  mutate( correct_binomial = ifelse( is.na(correct_binomial), F, correct_binomial),  
          correct_genus = ifelse( is.na(correct_genus), F, correct_genus), 
          correct_family = ifelse( is.na(correct_family), F, correct_family)) %>% 
  rowwise() %>% 
  mutate( score = (correct_binomial + correct_genus + correct_family) ) %>%  # CALCULATE SCORE 
  mutate( score = ifelse( is.na(key), NA, score ))  %>%                # Score is NA where no key_taxa are provided
  ungroup() %>% 
  distinct()

# Show any repeats that exist 
responses %>% 
  group_by( id, filename,  created_at, filename, response, score ) %>% 
  filter( n() > 1 ) 

responses %>% 
  write_csv('output/cleaned_response_data.csv')

