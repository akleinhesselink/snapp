rm(list = ls())

library(brms) 
library(tidyverse) 

load('output/fit_2pl.null_key_item.rda')
m_null <- fit_null_key_item
rm(fit_null_key_item)

load( 'output/full_model_fit_1.rda')
m1 <- temp_fit

load( 'output/full_model_fit_2.rda')
m2 <- temp_fit

load( 'output/full_model_fit_3.rda')
m3 <- temp_fit

load( 'output/full_model_fit_4.rda')
m4 <- temp_fit

load( 'output/full_model_fit_5.rda')
m5 <- temp_fit

load( 'output/full_model_fit_6.rda')
m6 <- temp_fit

load( 'output/full_model_fit_7.rda')
m7 <- temp_fit

load( 'output/full_model_fit_8.rda')
m8 <- temp_fit

### 
loo_null <- loo( m_null )
loo_1 <- loo( m1 )
loo_2 <- loo( m2 )
loo_3 <- loo( m3 )
loo_4 <- loo( m4 )
loo_5 <- loo( m5 )
loo_6 <- loo( m6 )
loo_7 <- loo( m7 )
loo_8 <- loo( m8 )

loo_compare(loo_null, loo_1, loo_2, loo_3, loo_4, loo_5, loo_6, loo_7, loo_8)


shinystan::launch_shinystan(m4)


