rm(list = ls())

library(brms) 
library(tidyverse) 

load('output/fit_null.rda')

load( 'output/full_model_fit_1.rda')
m1 <- temp_fit
rm(temp_fit )
loo_1 <- loo( m1 )
rm(m1)

load( 'output/full_model_fit_2.rda')
m2 <- temp_fit
rm(temp_fit )
loo_2 <- loo( m2 )
rm(m2)

load( 'output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit )
loo_3 <- loo( m3 )
rm(m3)

load( 'output/full_model_fit_4.rda')
m4 <- temp_fit
rm(temp_fit )
loo_4 <- loo( m4 )
rm(m4)

load( 'output/full_model_fit_5.rda')
m5 <- temp_fit
rm(temp_fit )
loo_5 <- loo( m5 )
rm(m5)

load( 'output/full_model_fit_6.rda')
m6 <- temp_fit
rm(temp_fit )
loo_6 <- loo( m6 )
rm(m6)

load( 'output/full_model_fit_7.rda')
m7 <- temp_fit
rm(temp_fit )
loo_7 <- loo( m7 )
rm(m7)

load( 'output/full_model_fit_8.rda')
m8 <- temp_fit
rm(temp_fit )
loo_8 <- loo( m8 )
rm(m8)

### 
model_comparison <- loo_compare(loo_1, loo_2, loo_3, loo_4, loo_5, loo_6, loo_7, loo_8)
write_csv(model_comparison %>% as.data.frame(), path = 'output/model_comparison_table.csv')
