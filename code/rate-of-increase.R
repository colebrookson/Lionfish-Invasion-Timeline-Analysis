########## 
##########
# This code contains the analysis for the rate of change estimation presented
# in Linardich et al. (2020) 
# "Trait-based vulnerability framework reveals the impact of a global marine invader" 
# This is the first of n code files for this analysis
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-09-14
##########
#########

library(tidyverse)
library(here)

#note that this data is private and also too large to put on github, so cannot put it in a repo
dir = "C:/Users/coleb/Google Drive/fish vulnerability to lionfish predation project_Christi and Stephanie/Cole's files/Cole_Old_Model_Fitting"
setwd(dir)
reef_abund_full = read_csv('REEF_abundance_full.csv',
                           guess_max = 200000)

#make single date column
reef_abund_full$date = as.Date(with(reef_abund_full, paste(year, month, day, sep = '-')), 
                             '%Y-%m-%d')

#keep only lionfish
reef_abund_lf = reef_abund_full %>% 
  filter(Species == '683')

#find first observation in each subregion
reef_abund_lf_minmax = reef_abund_lf %>% 
  group_by(subregion) %>%
  filter(Abundance > 0) %>% 
  summarize(min_date = min(date))

#NOTE --- decided to exclude any subregions which were invaded after 2015 (will do this later in the script)

#find first largest observation
reef_abund_lf_minmax = reef_abund_lf %>% 
  group_by(subregion) %>%
  filter(Abundance > 0) %>% 
  summarize(min_date = min(date),
            max_abund = max(Abundance)) %>% 
  filter(min_date < '2015-01-01') %>%  #this isn't technically needed but keeping it for posterity
  filter(max_abund > 2)

#get date of the first largest abundance observation
#could probably do this tidy, but I'm going to use a forloop
reef_abund_lf_minmax$max_date = NA
reef_abund_lf_minmax$max_date = as.Date(reef_abund_lf_minmax$max_date)

for(i in unique(reef_abund_lf_minmax$subregion)) {
  
  abund = reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'max_abund'][[1]]
  
  df = reef_abund_full %>% 
    filter(subregion == i) %>% 
    filter(Species == 683) %>% 
    filter(Abundance == abund)
  
  reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'max_date'] = min(df$date)
  
  
}























