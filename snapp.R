# R script pipeline for replicating analyses in 
# "Crowdsourcing snake identification with online communities of professionals and avocational enthusiasts"

require( rstan )
require( bayesplot )
require( tidyverse )
require( boot )
require( brms )
require( loo )


# Steps for replicating analyses
# First six scripts need to be run in order 

source('code/prepare_data.R') # Load and join relevant data 

source('code/split_training_testing.R') # split data into training and testing and save for analyses 

source('code/fit_IRT.R' )  # !!! -- Loop through and fit candidate models. May take several DAYS to run -- !!!!!!!

source('code/model_comparison.R')  # Calculate LOO for each model -- takes an hour or more. 

source('code/eval_fit.R')  # Evalutate the fit of each model on out of sample data -- takes an hour or more. 

source('code/make_model_comparison_table.R') # summarise fit of all candidate models 

# generate Plots 
source('code/predictive_check.R')
source('code/replot_main_effects.R') # plot effects for top model  (model 3)
source('code/plot_difficulty.R')
source('code/plot_discrimination.R')
source('code/plot_taxa_repeat_effect.R')

# Additional miscellaneous scripts included: 
# "code/summary_stats.R" : exploratory analysis
# "code/show_duplicate_responses.R" : cleanup errors in data recording 

