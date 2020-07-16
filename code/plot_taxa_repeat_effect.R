# choose reference family
rm(list =ls())
library(brms)
library(tidyverse)
library(ggridges)
library(boot)

pw <- 10 # print width 
ph <- 5 # print height 

load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

my_cols <- RColorBrewer::brewer.pal(3, "Set2")

# choose reference family
fams <- sort( unique( m3$data$key_family ) )
regs <- sort( unique( m3$data$photo_region))

my_conditions <- 
  expand.grid( home_region = c(T,F), 
               taxa_repeat = seq(1,10, by = 1), 
               key_family = fams, 
               photo_region = regs ) 

mu <- posterior_linpred( m3, newdata = my_conditions, re_formula = NA)
disc <- exp(posterior_samples( m3, 'b_disc_Intercept')[,1])
Int <- posterior_samples( m3, 'b_Intercept')

p_one    <- inv.logit( disc*(Int[, 1] - mu) ) 
p_two    <- inv.logit( disc*(Int[, 2] - mu) ) - inv.logit( disc*( Int[,1] - mu )) 
p_three  <- inv.logit( disc*(Int[, 3] - mu) ) - inv.logit( disc*( Int[,2] - mu ))
p_four   <- 1 - inv.logit( disc*(Int[, 3] - mu ))

p_fam <- p_two + p_three + p_four  
p_gen <- p_three + p_four 
p_spe <- p_four 

qs <- c(0.5, 0.025, 0.975) # quantiles to save 

accuracy_df <- 
  data.frame( 
    my_conditions, 
    bind_rows(
      data.frame( Accuracy = 'Family Correct', t( apply( p_fam, 2, quantile, qs) )), 
      data.frame( Accuracy = 'Genus Correct', t( apply( p_gen, 2, quantile, qs) )), 
      data.frame( Accuracy = 'Species Correct', t( apply( p_spe, 2, quantile, qs) ))
    )
  )

accuracy_df <- 
  accuracy_df %>% 
  mutate( `Identification Accuracy` = factor( Accuracy, levels = c('Family Correct', 'Genus Correct', 'Species Correct'), ordered = T)) %>% 
  mutate( `Family` = key_family, 
          Region = photo_region, 
          Home = factor( home_region, 
                         labels = c('Outside Home Region', 'Inside Home Region')))

scaleFUN <- function(x) sprintf("%.0f", x) # for scaling X axis 

repeat_plots <- list( )

for( i in 1:length(regs)){ 
  
  repeat_plots[[i]] <- list()  
  
  for( j in 1:length(fams)){ 
    
    repeat_plots[[i]][[j]] <- 
      accuracy_df %>%
      filter( Region == regs[i], Family == fams[j]) %>% 
      ggplot( aes( x = taxa_repeat, y = X50., ymin = X2.5., ymax = X97.5., fill = `Identification Accuracy`,color = `Identification Accuracy`, group = `Identification Accuracy`)) + 
      geom_line(size = 1, alpha = 0.7) + 
      geom_ribbon(alpha = 0.7) + 
      facet_wrap( ~ Home ) + 
      scale_color_manual(values = my_cols) + 
      scale_fill_manual( values = my_cols) + 
      ylim(0,1) +
      xlim(1, 10) + 
      scale_x_continuous(labels = scaleFUN) +   
      ylab( 'Probability') + 
      xlab( 'Exposure to taxa (# of images seen)') 
    
    names(repeat_plots[[i]])[j] <- fams[j]
  }
  names(repeat_plots)[i] <- regs[i]
}

saveRDS(repeat_plots, 'output/repeat_plots.RDS')

repeat_plots$`North America`$Colubridae +
  ggsave(filename = 'figures/Taxa_repeat_effect.png', height = ph, width = pw)
