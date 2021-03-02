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
library(glmmTMB)
library(DHARMa)
library(PNWColors)
library(MuMIn)

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
rm(temp_allvars, temp_join, temp_rank)

# make new diet variable in a new df
reef_allvars_diet = reef_allvars %>% 
  mutate(diet = as.integer(NA))

# join rank to the allvars dataframe (going to do this in a loop)
for(i in 1:nrow(reef_allvars)) {
  
  temp_rank = region_rank %>% 
    filter(region_name == reef_allvars[i,"region_name"][[1]],
           species_name == reef_allvars[i, "species_name"][[1]])
  
  reef_allvars_diet[i,"diet"] = ifelse(nrow(temp_rank) > 0, 1, 0)
  
}
rm(temp_rank)
# ordinal regression ===========================================================

reef_abund_rank$rank = as.factor(as.character(reef_abund_rank$rank))
ordinal_model = polr(rank ~ vul_score + abundance + 
                       vul_score*abundance, 
                     data = reef_abund_rank,
                     Hess = TRUE)
summary(ordinal_model)
confint(ordinal_model)

#### BEGIN NOTE ################################################################
# So the ordinal regression is a horrible model, which is not that unexpected
# since there are only 18 observations. Cannot currently think of a way to 
# get around this, so will probably not report the results for this. 
#### END NOTE ##################################################################

# binomial regression ==========================================================

reef_allvars_diet$vul_score = 
  as.numeric(reef_allvars$vul_score)
logistic_model_full = glmmTMB(diet ~ abundance + vul_score + 
                       abundance*vul_score, 
                     family = "binomial", 
                     data = reef_allvars_diet)

model_dredge = dredge(logistic_model_full)
logistic_model_noint = glmmTMB(diet ~ abundance + vul_score, 
                              family = "binomial", 
                              data = reef_allvars_diet)

# diagnose all models
summary(logistic_model_full)
plotQQunif(logistic_model_full)
plotResiduals(logistic_model_full)
testResiduals(logistic_model_full)
summary(logistic_model_noint)
plotQQunif(logistic_model_noint)
plotResiduals(logistic_model_noint)
testResiduals(logistic_model_noint)
# model fits fine but not great

# get the estiamtes
estimates = cbind(Estimate = coef(logistic_model_full), 
                  confint(logistic_model_full))


# get model predictions
new_data = data.frame(
  vul_score = rep(0:7, each = 100),
  abundance = rep(seq(from = min(reef_allvars_diet$abundance), 
                      to = max(reef_allvars_diet$abundance), 
                      length.out = 100), 
                  8))
new_data = cbind(new_data, predict(logistic_model_full, 
                                   new_data, 
                                   type = "response",
                                   se.fit = TRUE)) %>% 
  rename(fit_full = fit, se.fit_full = se.fit)
new_data = cbind(new_data, predict(logistic_model_noint, 
                                   new_data, 
                                   type = "response",
                                   se.fit = TRUE)) %>% 
  rename(fit_noint = fit, se.fit_noint = se.fit)

new_data = new_data %>% 
  mutate(
    UL_full = ifelse((fit_full + 1.96 * se.fit_full) > 1, 1, 
                     (fit_full + 1.96 * se.fit_full)), 
    LL_full = ifelse((fit_full - 1.96 * se.fit_full) < 0, 0, 
                     (fit_full - 1.96 * se.fit_full)),
    UL_noint = ifelse((fit_noint + 1.96 * se.fit_noint) > 1, 1, 
                     (fit_noint + 1.96 * se.fit_noint)), 
    LL_noint = ifelse((fit_noint - 1.96 * se.fit_noint) < 0, 0, 
                     (fit_noint - 1.96 * se.fit_noint)),
    
    fit = (fit_full*0.305 + fit_noint*0.693),
    UL = (UL_full*0.305 + UL_noint*0.693),
    LL = (LL_full*0.305 + LL_noint*0.693)
  )
  

  


# change vul_score = 
new_data$vul_score = as.factor(as.character(new_data$vul_score))
pal = pnw_palette("Bay", 8)
predicted_consumption = new_data %>% 
  #filter(vul_score %in% c(1, 3, 7)) %>% 
  ggplot(.,
       aes(abundance, fit)) +
  geom_line(aes(colour = vul_score), 
            size = 2) +
  geom_ribbon(aes(ymin = LL, 
                  ymax = UL, 
                  fill = vul_score), 
              alpha = 0.25) +
  labs(x = "Abundance", y = "Predicted Consumption") +
  theme_bw() +
  scale_colour_manual("Vulnerability \nScore", values = pal) +
  scale_fill_manual("Vulnerability \nScore", values = pal)
ggsave(here('/figures/predicted_consumption_model_prediction_small.png'),
      predicted_consumption, dpi = 200)
ggsave(here('/figures/predicted_consumption_model_prediction_large.png'),
       predicted_consumption, dpi = 600)


○ggsave(here('./figures/hist_and_timeseries_opt2_small.png'), height = 6, width = 13,
       figure_7, dpi = 200)
ggsave(here('./figures/hist_and_timeseries_opt2_large.png'), height = 6, width = 13,
       figure_7, dpi = 600)








