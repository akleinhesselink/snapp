rm(list =ls())
library(brms)
library(tidyverse)
library(ggridges)
library(boot)

load('output/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

pw <- 8 # image width 
ph <- 8 # image height 

my_cols <- RColorBrewer::brewer.pal(3, "Set2")
qs <- c(0.5, 0.025, 0.975) # quantiles to save 

# choose reference family
fams <- sort( unique( m3$data$key_family ) )
regs <- sort( unique( m3$data$photo_region))

my_conditions <- 
  expand.grid( home_region = c(T,F), 
               taxa_repeat = 1, 
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

score_df <-  
  data.frame( 
    my_conditions, 
    bind_rows(
      data.frame( Score = 1, t( apply( p_one, 2, quantile, qs) )), 
      data.frame( Score = 2, t( apply( p_two, 2, quantile, qs) )), 
      data.frame( Score = 3, t( apply( p_three, 2, quantile, qs) )), 
      data.frame( Score = 4, t( apply( p_four, 2, quantile, qs) ))
    )
  )

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
                           labels = c('Outside Home Region', 'Inside Home Region') ))  

family_plots <- list()

for( i in 1:length(regs) ){ 

  family_plots[[i]] <- 
    accuracy_df %>% 
    filter( Region == regs[i]) %>%
    ggplot( aes( x = Family, y = X50., ymin = X2.5., ymax = X97.5., 
                 color = `Identification Accuracy`, 
                 group = `Identification Accuracy`)) + 
    geom_errorbar(position = position_dodge(width = 0.5), alpha = 1, size = 0.7, width = 0.4) + 
    geom_point(position = position_dodge(width = 0.5), size = 4, color = 'grey92') + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.5) + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 1, shape = 1) + 
    facet_wrap( ~ Home) + 
    scale_color_manual(values = my_cols) + 
    ylab( 'Probability') + 
    ylim(0,1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  names(family_plots)[i] <- regs[i] 
}

region_plots <- list()

for( i in 1:length(fams) ){ 
  
  region_plots[[i]] <- 
    accuracy_df %>% 
    filter( key_family == fams[i]) %>%
    ggplot( aes( x = Region, y = X50., ymin = X2.5., ymax = X97.5., 
                 color = `Identification Accuracy`, 
                 group = `Identification Accuracy`)) + 
    geom_errorbar(position = position_dodge(width = 0.5), alpha = 1, size = 0.7, width = 0.4) + 
    geom_point(position = position_dodge(width = 0.5), size = 4, color = 'grey92') + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.5) + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 1, shape = 1) + 
    facet_wrap( ~ Home) + 
    scale_color_manual(values = my_cols) + 
    ylab( 'Probability') + 
    ylim(0,1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  names(region_plots)[i] <- fams[i]
}

saveRDS(region_plots, 'output/region_plots.rds')
saveRDS(family_plots, 'output/family_plots.rds')

region_plots$Colubridae + ggsave('figures/Region_effect.png', width = pw, height = ph)
family_plots$`North America` + ggsave('figures/Family_effect.png', width = pw, height = ph)



# score_df %>%
#   mutate( Score = as.character(Score)) %>%
#   filter( photo_region == reference_region) %>%
#   ggplot( aes( x = key_family, y = X50., ymin = X2.5., ymax = X97.5.,
#                color = `Score`,
#                group = `Score`)) +
#   geom_point( position = position_dodge(width = 0.5)) +
#   geom_errorbar( position = position_dodge(width = 0.5)) +
#   facet_wrap( ~ home_region) +
#   ylab( 'Probability') +
#   ylim(0,1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# my_conditions <-
#   expand.grid( home_region = c(T, F), taxa_repeat = 1, photo_region = reference_region) %>%
#   make_conditions( vars = c('home_region', 'taxa_repeat', 'photo_region')) %>%
#   distinct()
# 
# test <- conditional_effects(m3, effects = 'key_family', conditions = my_conditions, categorical = T)
# 
# test$`key_family:cats__` %>%
#   mutate( score = as.numeric( cats__ ), home_region = as.logical(home_region)) %>%
#   select(key_family, home_region, taxa_repeat, photo_region, score, estimate__, lower__, upper__) %>%
#   left_join(score_df %>% mutate( score = Score)) %>%
#   filter( estimate__ == X50.)
# 
