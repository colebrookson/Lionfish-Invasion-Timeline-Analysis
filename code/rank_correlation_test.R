
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
rank_data$IRI = as.factor(rank_data$IRI)
summary(rank_data$cum_vulnerab_score)
rank_corr = polr(IRI ~ cum_vulnerab_score, data = rank_data, Hess = TRUE)
summary(rank_corr)

#get a combined table 
coef_table = coef(summary(rank_corr))
p_values = pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE)*2
coef_table = cbind(coef_table, p_values, "p_value" = p_values)

#get the CIs
conf_int = confint(rank_corr)
confint.default(rank_corr) #assuming normality
exponents = exp(cbind(odds_ratio = coef(rank_corr), conf_int))

#let's try it with a spearman's correlation test


