setwd('~/Dropbox/projects/snake_survey/')
rm(list = ls()) 
library(brms)
library(tidyverse)

load( 'output/fit_form3_no_taxa_repeat_slope_re.rda')

fit3_alt <- temp_fit

load('output/fit_2pl_model_3.rda')

fit3 <- temp_fit

fit3_alt$data
fit3$data

fit3$data 

plot(fit3)

summary(fit3)
summary(fit3_alt)

p3 <- predict(fit3, newdata = fit3_alt$data, summary = F)
p3_alt <- predict(fit3_alt, newdata = fit3_alt$data, summary = F)


y_true <- fit3_alt$data$score
y_true <- as.numeric( y_true )

lppd1 <- lppd2 <- NA

for( n in 1:length(y_true) ) { 
  lppd1[n] <- log( mean( p3[, n] == as.numeric( y_true[n] )  ) )
  lppd2[n] <- log( mean( p3_alt[, n] == as.numeric( y_true[n] )  ) )
}


sum(lppd1)
sum(lppd2)

lppd2
