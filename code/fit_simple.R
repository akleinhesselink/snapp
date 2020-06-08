rm(list = ls() )
library(brms)
library(tidyverse)
options(buildtools.check = function(action) TRUE )

load( 'output/training_testing_data.rda')
load( 'output/full_model_fit_1.rda')

m_full <- temp_fit
rm(temp_fit)

# 
# Sampling parms: 
my_iter <- 5000
my_cores <- 4 
my_thin <- 5 

form_simple1 <- bf(
  formula = score ~ key_mivs + key_family + photo_region + (1 | i | item) + (taxa_repeat | id ) +  (1 | key), disc ~ 1 + (1 | i | item )
  )

form_simple2 <- bf(
  formula = score ~ key_mivs + key_family + photo_region + difficulty  + (1 | i | item) + (taxa_repeat | id ) +  (1 | key), disc ~ 1 + (1 | i | item )
)

form_simple3 <- bf(
  formula = score ~ key_mivs + key_family + photo_region + home_region + (1 | i | item) + (taxa_repeat | id ) +  (1 | key), disc ~ 1 + (1 | i | item )
)

my_prior <- m_full$prior

fit_simple1 <- brm( form_simple1, 
                    data = train, 
                    family = brmsfamily("cumulative", "logit"),
                    prior = my_prior, 
                    cores = my_cores, 
                    iter = my_iter, 
                    thin = my_thin, 
                    seed = 1) 

save(fit_simple1, file = 'output/fit_simple_1.rda')
rm(fit_simple1)

fit_simple2 <- brm( form_simple2, 
                    data = train, 
                    family = brmsfamily("cumulative", "logit"),
                    prior = my_prior, 
                    cores = my_cores, 
                    iter = my_iter, 
                    thin = my_thin, 
                    seed = 1 ) 

save(fit_simple2, file = 'output/fit_simple_2.rda')
rm(fit_simple2)


fit_simple3 <- brm( form_simple3, 
                    data = train, 
                    family = brmsfamily("cumulative", "logit"),
                    prior = my_prior, 
                    cores = my_cores, 
                    iter = my_iter, 
                    thin = my_thin, 
                    seed = 1) 


save(fit_simple3, file = 'output/fit_simple_3.rda')
rm(fit_simple3)
