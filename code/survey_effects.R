rm(list = ls()) 
library(brms)
library(tidyverse)

load('output/training_testing_data.rda')
load('output/fit_2pl.null.rda')
load('output/fit_2pl_model_1.rda')

fit1 <- temp_fit
load('output/fit_2pl_model_2.rda')
fit2 <- temp_fit
load('output/fit_2pl_model_3.rda')
fit3 <- temp_fit
load('output/fit_2pl_model_4.rda')
fit4 <- temp_fit 
load('output/fit_2pl_model_5.rda')
fit5 <- temp_fit 
load('output/fit_2pl_model_6.rda')
fit6 <- temp_fit 
load('output/fit_2pl_model_7.rda')
fit7 <- temp_fit
load('output/fit_2pl_model_8.rda')
fit8 <- temp_fit 

summary( fit1 )
summary( fit2 )

fit1 <- add_criterion(fit1, 'loo')
fit2 <- add_criterion(fit2, 'loo')
fit3 <- add_criterion(fit3, 'loo')
fit4 <- add_criterion(fit4, 'loo')
fit5 <- add_criterion(fit5, 'loo')
fit6 <- add_criterion(fit6, 'loo')
fit7 <- add_criterion(fit7, 'loo')
fit8 <- add_criterion(fit8, 'loo')

loo_compare(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)

fit3$model

fit3$formula
fit4$formula

# model 3 and model 1 are the  best fitting models 
# they both include the taxa repeat main effect and hierarchical effect within user 

# model 4 is next in fit, it includes key_family, photo_region and home_region 

# compare prediction on oos testing data 

all(  (test$key_family %in% fit3$data$key_family ))

test_limited <- test[ test$key_family %in% unique( fit3$data$key_family ) , ] 

p3 <- predict( fit3, newdata = test_limited, allow_new_levels = T, summary = F)
p4 <- predict( fit4, newdata = test_limited, allow_new_levels = T, summary= F)

lppd1 <- lppd2 <- NA
for( n in 1:nrow(test_limited) ) { 
  lppd1[n] <- log( mean( p3[, n] == as.numeric( test_limited$score[n] )  ) )
  lppd2[n] <- log( mean( p4[, n] == as.numeric( test_limited$score[n] )  ) )
}

sum(lppd1)
sum(lppd2)

neff_ratio(fit3)
summary(fit3)
summary(fit4)

class( fit3$data$taxa_repeat )

stancode(fit3)

plot( fit3)

library(emmeans)
emmeans( fit3, score ~ photo_region + key_family ) 




