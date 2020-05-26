rm(list = ls()) 
library(brms)
library(tidyverse)

load('output/training_testing_data.rda')
load('output/fit_1pl.null.rda')
load('output/fit_2pl.null.rda')
load('output/fit_2pl_model_1.rda')

summary( fit_1pl.null )
summary( fit_2pl.null)

plot(fit_1pl.null)
plot(fit_2pl.null)


fit1 <- add_criterion(fit_1pl.null, 'loo')
fit2 <- add_criterion(fit_2pl.null, 'loo')


loo_compare(fit1, fit2, criterion = 'waic')
loo_compare(fit1, fit2, criterion = 'loo')

# Model 2pl is preferred but there are some sampling issues. 

predict(fit1, test)


# compare prediction on oos testing data 
oos_fit1 <- predict( fit1, test , allow_new_levels = T)
oos_fit2 <- predict( fit2, test , allow_new_levels = T)


oos_fit1 %>% head()

test$predicted1 <- oos_fit1 %*% 1:4
test$predicted2 <- oos_fit2 %*% 1:4 

plot( test$predicted, test$score)

test %>% 
  ggplot( aes( x = predicted1 )) + 
  geom_histogram() + 
  facet_wrap( ~ score, ncol = 1 ) 

test %>% 
  ggplot( aes( x = predicted2 )) + 
  geom_histogram() + 
  facet_wrap( ~ score, ncol = 1 ) 

# Posterior predictive check  in sample data

pp_check(fit_1pl.null, nsamples = 20)
pp_check(fit_2pl.null, nsamples = 20)

# Posterior predictive check out of sample testing data 
pp_check(fit_1pl.null, newdata = test, nsamples = 20)
pp_check(fit_2pl.null, newdata = test, nsamples = 20)

# Two parameter model is preferred. 

p1 <- predict( fit_1pl.null, newdata = test, allow_new_levels = T, summary = F)
p2 <- predict( fit_2pl.null, newdata = test, allow_new_levels = T, summary= F)

lppd1 <- lppd2 <- NA
for( n in 1:nrow(test) ) { 
  lppd1[n] <- log( mean( p1[, n] == as.numeric( test$score[n] )  ) )
  lppd2[n] <- log( mean( p2[, n] == as.numeric( test$score[n] )  ) )
}

sum(lppd1)
sum(lppd2)


