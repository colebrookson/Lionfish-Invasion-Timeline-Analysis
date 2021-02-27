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
library(MASS)

reef_abund = read_csv(here('./data/REEF_abundance_full.csv'), 
                      guess_max = 3000000)
reef_effort = read_csv(here('./data/REEF_effort_data_fromMaster.csv'))
region_rank = read_csv(here('./data/lionfish diet data_regional.csv'))
prey_vul = read_csv(here('./data/prey_vulnerability_updated.csv'))
species_codes = read_csv(here('./data/species_codes.csv'))


# prepare data for model fits ==================================================

# get the rank data and keep only the variables we need
region_rank = region_rank %>% 
  dplyr::select(`Species name in Linardich et al.`, 
         Region, Rank, `fish or invert?`) %>% 
  rename(species_name = `Species name in Linardich et al.`,
         region_name = Region, 
         rank = Rank,
         taxa = `fish or invert?`) %>% 
  filter(taxa == 'fish') %>% 
  dplyr::select(-taxa) %>% 
  filter(!is.na(species_name))
region_rank$rank = as.numeric(region_rank$rank)

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

# get rid of extra objects
rm(reef_effort_bahamas, reef_effort_belize,
   reef_effort_bermuda, reef_effort_mexico,
   reef_effort_negulf, reef_effort_nwgulf)

# keep only abundance values corresponding to the surveys filtered above
reef_abund = reef_abund %>% 
  filter(formid %in% unique(reef_effort$formid))

# join the abundance with some species names
reef_abund = merge(reef_abund, species_codes,
                   by.x = "Species", by.y = "REEF species ID#")

# make a dataframe with only what we need
reef_allvars = reef_abund %>% 
  dplyr::select(formid, region, subregion, 
         geogr, Abundance, `Scientific name`) %>% 
  rename(abundance = Abundance,
         species_name = `Scientific name`)
reef_allvars = merge(reef_allvars, prey_vul,
                     by.x = "species_name",
                     by.y = "Binomial") %>% 
  rename(vul_score = `Vulnerability score`)

# make new variable with regional name (for ease)
reef_allvars = reef_allvars %>% 
  mutate(region_name = ifelse(subregion == 55, 
                              "Belize",
                              ifelse(subregion == 11,
                                     "Bermuda",
                                     ifelse(subregion %in% c(41, 42, 43),
                                            "Bahamas",
                       ifelse(subregion == 54,
                              "Mexico",
                              ifelse(subregion == 21,
                                     "neGOM",
                                     ifelse(subregion == 24, 
                                            "nwGOM",
                                            NA)))))))

# summarize data across the regions
reef_allvars = reef_allvars %>%
  group_by(species_name, vul_score, region_name) %>% 
  summarize(abundance = mean(abundance))

# first make a dataframe to append to
reef_abund_rank = data.frame(species_name = character(),
                             vul_score = integer(), 
                             abundance = numeric(), 
                             region_name = character(), 
                             rank = integer())

# join rank to the allvars dataframe (going to do this in a loop)
for(i in unique(reef_allvars$region_name)) {
  
  # filter the two dataframes and create temporary versions
  temp_allvars = reef_allvars %>% 
    filter(region_name == i) %>% 
    dplyr::select(-region_name)
  temp_rank = region_rank %>% 
    filter(region_name == i)
  
  # join the two temp dataframes
  temp_join = merge(temp_allvars, temp_rank, 
                    by.x = "species_name",
                    by.y = "species_name",
                    )
  
  # append the temp join to the dataframe already initialized
  reef_abund_rank = rbind(reef_abund_rank, 
                          temp_join)
  
}

# ordinal regression ===========================================================

reef_abund_rank$rank = as.factor(as.character(reef_abund_rank$rank))
ordinal_model = polr(rank ~ vul_score + abundance + 
                       vul_score*abundance, 
                     data = reef_abund_rank,
                     Hess = TRUE)
summary(ordinal_model)
confint(ordinal_model)
