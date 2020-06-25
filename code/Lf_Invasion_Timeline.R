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
#########

library(tidyverse)
library(lubridate)
library(ggsci)
library(PNWColors)
library(here)
`%notin%` = Negate(`%in%`)

lf_appear_subregs = read_csv(here('./data/lf_appear_subregs.csv'))

status_colors = pnw_palette("Bay", 8, type = 'continuous') #set your colours

#add a month buffer to the timeline and display the years
month_buffer = 2
str(lf_appear_subregs)
lf_appear_subregs$date = as.Date(lf_appear_subregs$date)
month_date_range = seq(min(lf_appear_subregs$date) - months(month_buffer), 
                       max(lf_appear_subregs$date) + months(month_buffer), 
                       by = 'month')
month_format = format(month_date_range, '%b')
month_df = data.frame(month_date_range, month_format)

# Show year text
year_date_range <- seq(min(lf_appear_subregs$date) - months(month_buffer), 
                       max(lf_appear_subregs$date) + months(month_buffer), 
                       by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

#actual plotting 
timeline_plot = ggplot(lf_appear_subregs,aes(x=date,y=0, col=region_name, 
                                           label=subregion)) +
  labs(col="Regions") +
  scale_color_manual(values=status_colors,drop = FALSE) +
  theme_classic() +
  geom_hline(yintercept=0, 
             color = "black", size=0.5) +
  geom_segment(data=lf_appear_subregs, 
               aes(y=position,yend=0,xend=date), 
               color='black', size=0.4) +
  geom_point(aes(y=0), size=4, shape = 19) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),        
        legend.title = element_text(size = 16), 
        #plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 14),
        axis.line.x =element_blank(),
        legend.position = 'bottom') +
  geom_text(data=year_df, 
            aes(x=year_date_range,y=-0.2,
                label=year_format, fontface="bold"),
            size=4, colour = 'black') +
  geom_text(aes(y=text_position,label=subreg_name),
            size=4)

ggsave(here('./figures/invasion_timeline.png'), timeline_plot, dpi = 1200, width = 13, 
       height = 7)
▼