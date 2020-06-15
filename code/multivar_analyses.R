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
##########

#set up
library(tidyverse)
library(vegan)

dir = "C:/Users/brookson/Documents/Github/Lionfish-Invasion-Timeline-Analysis/data"
setwd(dir)

multivar_data = read_csv('prey_data_for_multivar.csv')

#add categorical column for cumulative ranking
multivar_data = multivar_data %>% 
  mutate(cat_vulnerab_score = 
           ifelse(cum_vulnerab_score %in% c(0,1,2,3),'Low', 
                  ifelse(cum_vulnerab_score == 4, 'Med', 
                         ifelse(cum_vulnerab_score %in% c(5,6), 'High', NA))))
    
    
    
  






