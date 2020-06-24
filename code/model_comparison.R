rm(list = ls())

library(brms) 
library(tidyverse) 

load( 'output/full_model_fit_1.rda')
m1 <- temp_fit
rm(temp_fit )
names(m1)   
m1$fit@model_name <- 'm1'
m1 <- add_criterion(m1, criterion = 'loo')

load( 'output/full_model_fit_2.rda')
m2 <- temp_fit
rm(temp_fit )
m2$fit@model_name <- 'm2'
m2 <- add_criterion(m2, criterion = 'loo')

load( 'output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit )
m3$fit@model_name <- 'm3'
m3 <- add_criterion(m3, criterion = 'loo')

load( 'output/full_model_fit_4.rda')
m4 <- temp_fit
rm(temp_fit )
m4$fit@model_name <- 'm4'
m4 <- add_criterion(m4, criterion = 'loo')

load( 'output/full_model_fit_5.rda')
m5 <- temp_fit
rm(temp_fit )
m5$fit@model_name <- 'm5'
m5 <- add_criterion(m5, criterion = 'loo')

load( 'output/full_model_fit_6.rda')
m6 <- temp_fit
rm(temp_fit )
m6$fit@model_name <- 'm6'
m6 <- add_criterion(m6, criterion = 'loo')

### 
loo_compare( m1, m2, m3, m4)
summary(m3)
model_comparison <- loo_compare(loo_1, loo_2, loo_3, loo_4, loo_5, loo_6, criterion = 'loo')
write_csv(model_comparison %>% as.data.frame(), path = 'output/model_comparison_table.csv')
