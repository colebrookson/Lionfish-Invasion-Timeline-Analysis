########## 
##########
# This code contains the analysis for the linear model testing our proposed
# vulnerability framework that is developed and then implemented
# in Linardich et al. (2021) "Trait-based vulnerability framework reveals the 
# impact of a global marine invader"  
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-15
##########
##########

# set-up =======================================================================

library(here)
library(tidyverse)

reef_abund = read_csv(here('./data/REEF_abundance_full.csv'), 
                      guess_max = 3000000)
reef_effort = read_csv(here('./data/REEF_effort_data_fromMaster.csv'))
region_rank = read_csv(here('./data/lionfish diet data_regional.csv'))
prey_vul = read_csv(here('./data/prey_vulnerability_updated.csv'))

# prepare data for model fits ==================================================


# make new column with first four digits of the `geogr` column
reef_effort = reef_effort %>% 
  mutate(geo_subregion = substr(geogr, 1, 4))

# Only keep surveys from the areas we want from the times we want
reef_effort_bahamas = reef_effort %>% 
  filter(subregion %in% c(41, 42, 43),
         year %in% c(2003, 2007:2015))
reef_effort_nwgulf = reef_effort %>% 
  filter(subregion == 24,
         year %in% c(2011:2015))
reef_effort_negulf = reef_effort %>% 
  filter(subregion == 21,
         year %in% c(2013:2015))
reef_effort_mexico = reef_effort %>% 
  filter(subregion == 54,
         year %in% c(2010, 2013, 2015))
reef_effort_belize = reef_effort %>% 
  filter(geo_subregion == 5501,
         year %in% c(2011:2015))
reef_effort_bermuda = reef_effort %>% 
  filter(geo_subregion == 1101,
         year %in% c(2007:2010, 2013:2015))

reef_effort = rbind(reef_effort_bahamas, reef_effort_belize,
                    reef_effort_bermuda, reef_effort_mexico,
                    reef_effort_negulf, reef_effort_nwgulf)

# keep only abundance values corresponding to the surveys filtered above
reef_abund = reef_abund %>% 
  filter(formid %in% unique(reef_effort$formid))









