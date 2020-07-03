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
model_comparison <- loo_compare(m1, m2, m3, m4, m5, m6, criterion = 'loo')

model_comparison <- model_comparison %>% as.data.frame() %>% mutate( model_name = row.names(.) )

model_comparison$formula <- lapply( model_comparison$model_name, function(x) eval(parse(text = x))$formula )

model_comparison$formula <- unlist( lapply( model_comparison$formula, function(x) as.character( x$formula[[3]][2] )  ) )

write_csv(model_comparison, path = 'output/model_comparison_table.csv')
