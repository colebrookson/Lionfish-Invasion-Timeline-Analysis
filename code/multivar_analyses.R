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
multivar_data_zero_excluded = multivar_data %>% 
  filter(cum_vulnerab_score != 0)
summary(multivar_data)

#separate trait data from categorical data
traits_data = data.frame(multivar_data_zero_excluded[,6:11])
species_names = gsub(' ', '_', multivar_data_zero_excluded$binomial)
rownames(traits_data) = species_names

grouping_data = data.frame(multivar_data_zero_excluded[,c(5,12,13)])
rownames(grouping_data) = species_names
#try initial MDS
set.seed(123)
nmds_jaccard = metaMDS(traits_data,
                       distance = 'jaccard',
                       trymax = 100,
                       k = 2,
                       trace = FALSE)
plot(nmds_jaccard)
ordiplot(nmds_jaccard, type = 'n')
group = grouping_data$cat_vulnerab_score
for(i in unique(grouping_data$cat_vulnerab_score)) {
  ordihull(nmds_jaccard$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=T) }
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



