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
            max_abund = max(Abundance),
            min_abund = min(Abundance)) %>% 
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
rm(df, abund) #I know this is bad practice, but doing it anyways

#Find midpoint for each subregion
reef_abund_lf_minmax = reef_abund_lf_minmax %>% 
  mutate(mid_date = min_date + ((max_date - min_date)/2))

#find average abundance (if any recorded) for that month
reef_abund_lf_minmax$mid_abund = NA
reef_abund_lf_minmax$mid_abund = as.numeric(reef_abund_lf_minmax$mid_abund)

for(i in unique(reef_abund_lf_minmax$subregion)) {
  
  year_mid = as.numeric(substr(as.character(reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'mid_date'][[1]]),
                start = 1, stop = 4))
  month_mid = as.numeric(substr(as.character(reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'mid_date'][[1]]),
                start = 6, stop = 7))
                
  
  df = reef_abund_full %>% 
    filter(subregion == i) %>% 
    filter(Species == 683) %>% 
    filter(year == year_mid) %>% 
    filter(month == month_mid)
  
  if(nrow(df)>0) {
    
    reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'mid_abund'] = ceiling(mean(df$Abundance))
      
    } else { # give NA if no rows
    
    reef_abund_lf_minmax[which(reef_abund_lf_minmax$subregion == i), 'mid_abund'] = NA
    
    }
  
}
rm(df,year_mid,month_mid)

#get measurement of time to max abundance
reef_abund_lf_minmax$time_max_abund = ((reef_abund_lf_minmax$max_date - reef_abund_lf_minmax$min_date)/365)

#make histogram of times to max abundance
time_max_abund_plot = ggplot(data = reef_abund_lf_minmax) +
  geom_histogram(aes(x = time_max_abund), binwidth = 1, position = 'dodge',
                 colour = 'black', fill = 'goldenrod3') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  scale_x_continuous(breaks = c(0:12), labels = c(0:12)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  labs(x = 'Time Elapsed Between First Invasion and Maximum Abundance (Years)',
       y = ' Number of Subregions') 
ggsave(here('./figures/time_max_abund_plot_small.png'),
       time_max_abund_plot, dpi = 200)
ggsave(here('./figures/time_max_abund_plot.tiff'),
       time_max_abund_plot)

#get rate of increase by dividing midpoint abundance by time to max abundance
reef_abund_lf_minmax$rate_of_inc = 
  reef_abund_lf_minmax$mid_abund/as.numeric(reef_abund_lf_minmax$time_max_abund)



















