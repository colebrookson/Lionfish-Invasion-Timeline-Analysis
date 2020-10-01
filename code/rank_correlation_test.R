
########## 
##########
# This code contains the analysis for the rank abundance test presented in 
# in Linardich et al. (2020) 
# "Trait-based vulnerability framework reveals the impact of a global marine invader" 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-06-15
##########
#########

library(tidyverse)
library(PNWColors)
library(here)
library(MASS)
library(BiodiversityR)
`%notin%` = Negate(`%in%`)

rank_data = read_csv(here('./data/diet_vulnerability_rank_corr_data.csv'))
vulner_data = read_csv(here('./data/prey_data_for_multivar.csv'))

#get rid of leading whitespace in the species rank_data
rank_data$Species = as.character(rank_data$Species)
str(rank_data$Species)
rank_data$Species = trimws(rank_data$Species, which = c('left'))

rank_data = merge(rank_data, vulner_data,
                    by.x = 'Species', by.y = 'binomial')
rank_data$cum_vulnerab_score = as.factor(rank_data$cum_vulnerab_score)

plot(x = rank_data$cum_vulnerab_score, y = rank_data$IRI)
points(x = rank_data$cum_vulnerab_score, y = rank_data$IRI)

#to look at our question, we're going to use an ordinal logistic regression, because we're using a categorical variable 
#which is the vulnerability, to predict the position of each species in the output table. 

#the fit is simple:
# rank_data$IRI = as.factor(rank_data$IRI)
# summary(rank_data$cum_vulnerab_score)
# rank_corr = polr(IRI ~ cum_vulnerab_score, data = rank_data, Hess = TRUE)
# summary(rank_corr)
# 
# #get a combined table 
# coef_table = coef(summary(rank_corr))
# p_values = pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE)*2
# coef_table = cbind(coef_table, p_values, "p_value" = p_values)
# 
# #get the CIs
# conf_int = confint(rank_corr)
# confint.default(rank_corr) #assuming normality
# exponents = exp(cbind(odds_ratio = coef(rank_corr), conf_int))

#let's try it with a spearman's correlation test
rank_data$cum_vulnerab_score = as.numeric(as.character(rank_data$cum_vulnerab_score))
spearman_rank = cor.test(~IRI + cum_vulnerab_score,
                         data = rank_data,
                         method = 'spearman',
                         continuity = FALSE,
                         conf.level = 0.95)

#the kendall's tau-b might be better here
vulnerab_score = rank_data$cum_vulnerab_score; IRI = rank_data$IRI
rank_kendall_data = cbind(vulnerab_score, IRI)
kendall_rank = cor(rank_kendall_data, 
                   method = 'kendall',
                   use = 'pairwise')
kendall_corr = cor.test(vulnerab_score, 
                       IRI, 
                       method = 'kendall')
kendall_corr
plot(IRI ~ vulnerab_score)


#We'll try the biodiversity R package that just looks at rank abundance instead
rank_data$cum_vulnerab_score = as.factor(rank_data$cum_vulnerab_score)
rankabund = with(rank_data, lapply(levels(cum_vulnerab_score), function(lev)
  rankabundance(IRI, rank_data, 'cum_vulnerab_score', lev)))


#going to try another thing - taking the mean of each level of the vulnerability score, and plot that
level_vulnerab_data = rank_data %>% 
  group_by(cum_vulnerab_score) %>% 
  summarize(mean_iri = mean(IRI, na.rm = TRUE),
            mean_ioi = mean(IOI, na.rm = TRUE))
