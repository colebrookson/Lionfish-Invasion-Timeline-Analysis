########## 
##########
# This code contains the analysis for the multivariate statistics presented
# in Linardich et al. (2020) 
# "Trait-based vulnerability framework reveals the impact of a global marine invader" 
# This is the first of n code files for this analysis
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-15
##########
#########

library(tidyverse)
library(PNWColors)
library(here)
`%notin%` = Negate(`%in%`)

rank_data = read_csv(here('./data/diet_vulnerability_rank_corr_data.csv'))
vulner_data = read_csv(here('./data/prey_data_for_multivar.csv'))

#get rid of leading whitespace in the species rank_data
rank_data$Species = as.character(rank_data$Species)
str(rank_data$Species)
rank_data$Species = trimws(rank_data$Species, which = c('left'))

rank_data = merge(rank_data, vulner_data,
                    by.x = 'Species', by.y = 'binomial')

hist(rank_data$FOO)

#run logistic regression on the Frequency of Occurence continuous response variable

