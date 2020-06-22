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
library(here)
library(viridis)

multivar_data = read_csv(here('./data/prey_data_for_multivar.csv'))

#add categorical column for cumulative ranking
multivar_data = multivar_data %>% 
  mutate(cat_vulnerab_score = 
           ifelse(cum_vulnerab_score %in% c(0,1,2,3),'Low', 
                  ifelse(cum_vulnerab_score == 4, 'Med', 
                         ifelse(cum_vulnerab_score %in% c(5,6), 'High', NA))))
multivar_data_zero_excluded = multivar_data %>% 
  filter(cum_vulnerab_score != 0)
summary(multivar_data)

#separate trait data from categorical data
traits_data = data.frame(multivar_data_zero_excluded[,6:11])
species_names = gsub(' ', '_', multivar_data_zero_excluded$binomial)
rownames(traits_data) = species_names

grouping_data = data.frame(multivar_data_zero_excluded[,c(5,12,13)])
rownames(grouping_data) = species_names

#run mds analysis with k = 2 
set.seed(123)
nmds_jaccard_k2 = metaMDS(traits_data,
                       distance = 'jaccard',
                       trymax = 100,
                       k = 2,
                       trace = FALSE)

summary(nmds_jaccard_k2)
stressplot(nmds_jaccard_k2)
nmds_jaccard_k2$stress

## make plot for vulnerability
nmds_k2_vis_data_vulner = data.frame(scores(nmds_jaccard_k2)) 
nmds_k2_vis_data_vulner$species = rownames(nmds_k2_vis_data_vulner)  
nmds_k2_vis_data_vulner$vulner_score = grouping_data$cat_vulnerab_score  

split_nmds_k2_vulner = split(nmds_k2_vis_data_vulner, 
                             nmds_k2_vis_data_vulner$vulner_score)
applied_nmds_k2_vulner = lapply(split_nmds_k2_vulner, function(df){
  df[chull(df), ]
})
combined_nmds_k2_vulner = do.call(rbind, applied_nmds_k2_vulner)

#load in data on top most consumed species
species_data = read_csv(here('./data/top_species_from_peake_etal_2018.csv'))
species_data = species_data %>% 
  filter(rank <= 5)
point_vulner = nmds_k2_vis_data_vulner %>%
  filter(species %in% species_data$species)

point_vulner$species = gsub('_', ' ', as.character(point_vulner$species))

nmds_k2_vulner_plot 
ggplot() + 
  geom_polygon(data=combined_nmds_k2_vulner, 
               aes(x = NMDS1, y = NMDS2, 
                   fill= fct_relevel(vulner_score, 'Low', 'Med', 'High'), 
                   group=fct_relevel(vulner_score, 'Low', 'Med', 'High')), 
               alpha=0.30) + # add the convex hulls
  geom_point(data=combined_nmds_k2_vulner, 
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_relevel(vulner_score, 'Low', 'Med', 'High')), 
             size=2) + # add the point markers
  geom_jitter(data=point_vulner, 
             aes(x=NMDS1,y=NMDS2, 
                 colour = fct_relevel(vulner_score, 'Low', 'Med', 'High')), 
             size=3, shape = 15,
             #width = 0.05, height = 0.05,
             position = position_jitter(width = 0.03, height = 0.3, seed = 7)) + # add the point markers
  geom_label(data = point_vulner, 
            aes(x=NMDS1,y=NMDS2,
                 label = species, 
                colour = fct_relevel(vulner_score, 'Med', 'High')),
                #fill= fct_relevel(vulner_score, 'Low', 'Med', 'High'), 
                #group=fct_relevel(vulner_score, 'Low', 'Med', 'High')),
            size = 4, vjust= 0, hjust = 0,
            position = position_jitter(width = 0.03, height = 0.3, seed = 7),
            show.legend = FALSE)+
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 18),
        legend.justification = c(0,0.5),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_manual('Vulnerability', 
                    values = c('#006994', '#ffd300', '#ca3433')) +
  scale_colour_manual('Vulnerability', 
                      values = c('#006994', '#ffd300', '#ca3433')) +
  scale_colour_manual('Vulnerability', 
                      values = c('#006994', '#ffd300', '#ca3433'))
 
 
## make plot for vulnerability
nmds_k2_vis_data_diet = data.frame(scores(nmds_jaccard_k2)) 
nmds_k2_vis_data_diet$species = rownames(nmds_k2_vis_data_diet)  
nmds_k2_vis_data_diet$diet_study = grouping_data$diet_study 

split_nmds_k2_diet = split(nmds_k2_vis_data_diet, 
                           nmds_k2_vis_data_diet$diet_study)
applied_nmds_k2_diet = lapply(split_nmds_k2_diet, function(df){
  df[chull(df), ]
})
combined_nmds_k2_diet = do.call(rbind, applied_nmds_k2_diet)
combined_nmds_k2_diet = combined_nmds_k2_diet %>% 
  mutate(diet_cat = ifelse(combined_nmds_k2_diet$diet_study == 1, 
                           'Present', 'Absent'))

nmds_k2_plot_diet <- ggplot(data=combined_nmds_k2_diet) + 
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill= fct_relevel(diet_cat, 
                                                           'Absent', 'Present'), 
                   group=fct_relevel(diet_cat, 'Absent', 'Present')), 
               alpha=0.30) + # add the convex hulls
  geom_point(aes(x=NMDS1,y=NMDS2, colour = diet_cat), size=2) + # add the point markers
  coord_equal() +
  theme_bw()  +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 18),
        legend.justification = c(0,0.5),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_manual('Presence in Diet Studies', 
                    values = c('#000080', '#ed2939')) +
  scale_colour_manual('Presence in Diet Studies', 
                      values = c('#000080', '#ed2939'))







#################################### repear mds with 3d, but I don't think we'll use it
nmds_jaccard_k3 = metaMDS(traits_data,
                       distance = 'jaccard',
                       trymax = 100,
                       k = 3,
                       trace = FALSE)

summary(nmds_jaccard)
stressplot(nmds_jaccard)
nmds_jaccard$stress

nmds_vis_data = data.frame(scores(nmds_jaccard)) 
nmds_vis_data$species = rownames(nmds_vis_data)  
nmds_vis_data$vulner_score = grouping_data$cat_vulnerab_score  
nmds_vis_data$diet_study = grouping_data$diet_study 

