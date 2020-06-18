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

nmds_k2_vis_data = data.frame(scores(nmds_jaccard_k2)) 
nmds_k2_vis_data$species = rownames(nmds_k2_vis_data)  
nmds_k2_vis_data$vulner_score = grouping_data$cat_vulnerab_score  
nmds_k2_vis_data$diet_study = grouping_data$diet_study 

split_nmds_k2 = split(nmds_k2_vis_data, nmds_k2_vis_data$vulner_score)
applied_nmds_k2 = lapply(split_nmds_k2, function(df){
  df[chull(df), ]
})
combined_nmds_k2 = do.call(rbind, applied_nmds_k2)

## make plot for vulnerability
nmds_k2_plot <- ggplot(data=combined_nmds_k2) + 
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill= fct_relevel(vulner_score, 'Low', 'Med', 'High'), 
                   group=fct_relevel(vulner_score, 'Low', 'Med', 'High')), alpha=0.30) + # add the convex hulls
  geom_point(aes(x=NMDS1,y=NMDS2, colour = fct_relevel(vulner_score, 'Low', 'Med', 'High')), size=2) + # add the point markers
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
        #panel.background = element_rect(fill = "lightgrey"), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  scale_fill_manual('Vulnerability', values = c('#006994', '#ffd300', '#ca3433')) +
  scale_colour_manual('Vulnerability', values = c('#006994', '#ffd300', '#ca3433'))
  #guides(fill = FALSE, colour = FALSE) +
  #guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
  #scale_fill_viridis(option="magma", discrete = TRUE, begin = 0.8, end = 0.2, name = "Vulernability") + #name = "Global Change Assessed"
  #scale_colour_viridis(option="magma", discrete = TRUE, begin = 0.8, end = 0.2, name = "Vulernability") 

treat=nmds_k2_vis_data$vulner_score
ordiplot(nmds_jaccard_k2,type="n")
ordihull(nmds_jaccard_k2,groups=treat,draw="polygon",col=c('red', 'blue', 'green'),label=F, alpha = 0.4)
legend(x="bottomleft", legend=levels(nmds_k2_vis_data$vulner_score), col=c('red', 'blue', 'green'))
orditorp(nmds_jaccard_k2,display="species",col="red",air=0.01)
orditorp(nmds_jaccard_k2,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#































ggplot(data = nmds_k2_vis_data) +
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = vulner_score)) +
  geom_text(aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5)

ggplot() + 
  geom_polygon(nmds_k2_vis_data, aes(x = NMDS1,
                                     y = NMDS2, fill = vulner_score)),
                                     , alpha = 0.30) + # add the convex hulls
  geom_text(nmds_k2_vis_data, aes(x = NMDS1, 
                                  y = NMDS2,label = species), 
            alpha=0.5) +  # add the species labels
  geom_point(nmds_vis_data, aes(x = NMDS1, 
                                y = NMDS2, shape = grp, colour = grp),
             size = 4) + # add the point markers
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()







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


ggplot() + 
  geom_polygon(nmds_vis_data,aes(x = NMDS1,y = NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
  geom_text(nmds_vis_data,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(nmds_vis_data,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4) + # add the point markers
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()


plot(nmds_jaccard)
ordiplot(nmds_jaccard, type = 'p')
group = unique(grouping_data$cat_vulnerab_score)
for(i in unique(grouping_data$cat_vulnerab_score)) {
  ordihull(nmds_jaccard$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) 
}
orditorp(nmds_jaccard, display = "species", col = "red", air = 0.01)
orditorp(nmds_jaccard, display = "sites", col = c(rep("red",12),
                                                  rep("blue", 12)), air = 0.01, cex = 1.25)





data("varespec")

varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

dist <- vegdist(varespec,  method = "bray")

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), 
       xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", 
       ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, 
                                               autotransform = F, 
                                               k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)
set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F)
NMDS2
stressplot(NMDS2)
plot(NMDS1, type = "t")
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, 
                 autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

stressplot(NMDS2)

# Load the second dataset
data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(NMDS3, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)
# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Group1", 12), rep("Group2", 12))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)



