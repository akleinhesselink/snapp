rm(list =ls())
library(brms)
library(tidyverse)
library(ggridges)

pw <- 10 # print width 
ph <- 5 # print height 

load('output/cached_models/full_model_fit_3.rda')
m3 <- temp_fit
rm(temp_fit)

# choose reference family

fams <- sort( unique( m3$data$key_family ) )

ggp <- list()
for(i in 1:length(fams)){ 

  reference_family <- fams[i]

  my_conditions <- 
    expand.grid( home_region = c(T, F), taxa_repeat = 1, key_family = reference_family) %>% 
    make_conditions( vars = c('home_region', 'taxa_repeat', 'key_family')) %>% 
    distinct()

  cond_eff_df <- conditional_effects(m3, 'photo_region', conditions = my_conditions, categorical = T)
  
  cond_eff_df <- cond_eff_df$`photo_region:cats__`
  
  cond_eff_df$effect2__ <- factor( cond_eff_df$effect2__, labels = c('Incorrect', 'Family', 'Genus', 'Species'))
  
  cond_eff_df$cond__ <- factor( cond_eff_df$cond__, labels = c( 'Images from outside home region', 'Images from home region'))
  
  ggp[[i]] <- cond_eff_df  %>% 
    ggplot( aes( x = photo_region, y = estimate__, ymin = lower__, ymax = upper__, color = effect2__)) + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.7) + 
    geom_errorbar(position = position_dodge(width = 0.5), alpha = 0.7) + 
    facet_wrap( ~ cond__) + 
    scale_color_discrete(name = 'Identification Accuracy') + 
    ylab( 'Probability') + 
    xlab( 'Photo Region') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  names(ggp)[i] <- reference_family
}

saveRDS(ggp, 'output/region_plots.rds')

ggp$Colubridae + ggsave(filename = 'figures/Region_effect.png', width = pw, height = ph)

#
regs <- sort( unique( m3$data$photo_region))
ggp <- list()

for( i in 1:length(regs)){ 
  reference_region <- regs[i]
  my_conditions <- 
    expand.grid( home_region = c(T, F), taxa_repeat = 1, photo_region = reference_region) %>% 
    make_conditions( vars = c('home_region', 'taxa_repeat', 'photo_region')) %>% 
    distinct()

  cond_eff_df <- conditional_effects(m3, 'key_family', conditions = my_conditions, categorical = T)

  cond_eff_df <- cond_eff_df$`key_family:cats__`
  
  cond_eff_df$effect2__ <- factor( cond_eff_df$effect2__, labels = c('Incorrect', 'Family', 'Genus', 'Species'))
  
  my_labels <- c('Outside home region', 'Inside home region')
  cond_eff_df$cond__ <- factor( cond_eff_df$cond__, labels = my_labels)

  ggp[[i]] <-
    cond_eff_df  %>% 
    ggplot( aes( x = key_family, y = estimate__, ymin = lower__, ymax = upper__, color = effect2__)) + 
    geom_point(position = position_dodge(width = 0.5), size = 3, alpha = 0.7) + 
    geom_errorbar(position = position_dodge(width = 0.5), alpha = 0.7) + 
    facet_wrap( ~ cond__) + 
    scale_color_discrete(name = 'Identification Accuracy') + 
    ylab( 'Probability') + 
    xlab( 'Family') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

  names(ggp)[i] <- reference_region
}

ggp$`North America` + ggsave(filename = 'figures/Family_effect.png', width = pw, height = ph)
saveRDS(ggp, 'output/family_plots.rds')

# 
reference_region <- c('North America')
reference_family <- c('Colubridae')

my_conditions <- 
  expand.grid( home_region = c(F), photo_region = reference_region, key_family = reference_family) %>% 
  make_conditions( vars = c('home_region', 'photo_region', 'key_family')) %>% 
  distinct()

cond_eff_df <- conditional_effects(m3, 'taxa_repeat', conditions = my_conditions, categorical = T)

cond_eff_df <- cond_eff_df$`taxa_repeat:cats__`
cond_eff_df <- cond_eff_df %>% filter( taxa_repeat < 11, taxa_repeat > 0 )

cond_eff_df$effect2__ <- factor( cond_eff_df$effect2__, labels = c('Incorrect', 'Family', 'Genus', 'Species'))

cond_eff_df  %>% 
  ggplot( aes( x = taxa_repeat, y = estimate__, ymin = lower__, ymax = upper__, fill = effect2__),color = effect2__) + 
  geom_line(size = 1, alpha = 0.7) + 
  geom_ribbon(alpha = 0.7) + 
  scale_fill_discrete(name = 'Identification Accuracy') + 
  ylab( 'Probability') + 
  xlab( 'Exposure to taxa (# of images seen)') + 
  ggsave(filename = 'figures/Repeat_effect.png', width = pw/1.5, height = ph)





