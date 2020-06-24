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
`%notin%` = Negate(`%in%`)

reef_abund_full = read_csv(here('./data/REEF_abundance_full.csv'), 
                       guess_max = 1000000)


# So what we're trying to do here is estimate how fast LF populations grow once they 
#establish in a subregion - this will allow us to think about how much time you have
#to manage the invasion before it gets out of hand

#Step 1: Get the date of initial invastion
LF_all = reef_abund_full %>% 
  filter(Species == 683)

LF_all$date = with(LF_all, ymd(sprintf('%04d%02d%02d', year, month, day))) #make a date column
head(LF_all)

LF_dates = LF_all %>% 
  filter(lfabund != 0) %>% ## IMPORTANT! There are 4 regions that don't have any observations of lionfish - identify these later
  group_by(subregion) %>% 
  summarize(first_appearance = min(date))


LF_all_min = LF_all %>% 
  filter(lfabund != 0) %>% ## IMPORTANT! There are 4 regions that don't have any observations of lionfish - identify these later
  select(subregion, region, month, day, year, date, lfabund) %>% 
  left_join(LF_dates, by = "subregion") %>% 
  filter(date == first_appearance) %>% 
  distinct(.)

#Step 2: Get the first date of max occurence
LF_max = LF_all %>% 
  group_by(subregion) %>% 
  summarize(max_abund = max(lfabund))

LF_maxint = LF_all %>% 
  select(subregion, region, month, day, year, date, lfabund) %>% 
  left_join(LF_max, by = 'subregion') %>% 
  filter(lfabund == max_abund)
LF_maxint = LF_maxint %>% 
  group_by(subregion) %>% 
  summarize(max_first_appearance = min(date))
LF_maxab = LF_all %>% 
  filter(lfabund != 0) %>% 
  group_by(subregion) %>% 
  summarize(max_abund = max(lfabund))

LF_all_max = LF_all %>% 
  filter(lfabund != 0) %>% ## IMPORTANT! There are 4 regions that don't have any observations of lionfish - identify these later
  select(subregion, region, month, day, year, date, lfabund) %>% 
  left_join(LF_maxint, by = "subregion") %>% 
  filter(date == max_first_appearance) %>% 
  left_join(LF_maxab, by = 'subregion') %>% 
  filter(lfabund == max_abund) %>% 
  distinct(.)

#Step 3: Find the Midpoint of Abundance
LF_mid = LF_all_max %>% 
  select(subregion, max_abund, max_first_appearance) %>% 
  left_join(LF_all_min, by = 'subregion') %>% 
  select(-day, -year, -month) %>% 
  rename(min_abund = lfabund)

LF_mid = LF_mid %>% 
  mutate(interval = first_appearance %--% max_first_appearance) %>% 
  mutate(duration = as.duration(interval)) %>% 
  mutate(half_interval = duration/2) %>% 
  mutate(midpoint = round_date(first_appearance + half_interval, unit = 'day'))
LF_mid$midpoint = ymd(LF_mid$midpoint)
LF_mid = LF_mid[-20, ]

LF_mid_int = LF_mid %>% 
  select(-date) %>% 
  mutate(mid_date_m = round_date(midpoint, unit = 'month')) %>% 
  mutate(mid_date_d = round_date(midpoint, unit = 'day'))

LF_mid_abund = LF_all %>% 
  filter(lfabund != 0) %>% 
  select(subregion, date, lfabund) %>% 
  group_by(subregion) %>% 
  left_join(LF_mid_int, by = 'subregion') %>% 
  mutate(date_m = round_date(date, unit = 'month')) %>% 
  mutate(date_d = round_date(date, unit = 'day')) %>% 
  filter(date_m == mid_date_m) %>% 
  summarize(mid_abund = round(mean(lfabund)))

LF_mid = LF_mid %>% 
  left_join(LF_mid_abund, by = 'subregion')

LF_mid = LF_mid %>% 
  mutate(min_abund_scale = ifelse(min_abund == 0, 0,
                            ifelse(min_abund == 1, 2,
                                   ifelse(min_abund == 2, 10,
                                          ifelse(min_abund == 3, 100,
                                                 ifelse(min_abund == 4, 1000, NA)))))) %>% 
  mutate(max_abund_scale = ifelse(max_abund == 0, 0,
                                  ifelse(max_abund == 1, 2,
                                         ifelse(max_abund == 2, 10,
                                                ifelse(max_abund == 3, 100,
                                                       ifelse(max_abund == 4, 1000, NA)))))) %>% 
  mutate(mid_abund_scale = ifelse(mid_abund == 0, 0,
                                  ifelse(mid_abund == 1, 2,
                                         ifelse(mid_abund == 2, 10,
                                                ifelse(mid_abund == 3, 100,
                                                       ifelse(mid_abund == 4, 1000, NA)))))) 
LF_mid$duration_months = (LF_mid$duration/dyears(1))*12

#Step 4: Fit a curve and extract the rate of increase
LF_mid_only = LF_mid %>% 
  select(mid_abund, mid_abund_scale, midpoint, region, subregion, duration_months) %>% 
  rename(abund = mid_abund, abund_scale = mid_abund_scale, date = midpoint) %>% 
  mutate(time = 'mid')
LF_mid_only$duration_months = (LF_mid_only$duration_months)/2
LF_min_only = LF_mid %>% 
  select(min_abund, min_abund_scale, first_appearance, region, subregion) %>% 
  rename(abund = min_abund, abund_scale = min_abund_scale, date = first_appearance) %>% 
  mutate(time = 'min') %>% 
  mutate(duration_months = 0)
LF_max_only = LF_mid %>% 
  select(max_abund, max_abund_scale, max_first_appearance, region, subregion, duration_months) %>% 
  rename(abund = max_abund, abund_scale = max_abund_scale, date = max_first_appearance) %>% 
  mutate(time = 'max')

LF_time = rbind(LF_mid_only, LF_min_only, LF_max_only)
LF_time$subregion = as.factor(LF_time$subregion)

temp = LF_mid_only %>% 
  filter(!is.na(abund))
sb = unique(temp$subregion)
badsub = c(62,82,41,83)
LF_time = LF_time %>% 
  filter(subregion %in% sb) %>% 
  filter(subregion %notin% badsub)

LF_maxmin_max_only = LF_mid %>% 
  select(min_abund, min_abund_scale, first_appearance, region, subregion) %>% 
  rename(abund = min_abund, abund_scale = min_abund_scale, date = first_appearance) %>% 
  mutate(time = 'min') %>% 
  mutate(duration_months = 0)
LF_maxmin_min_only = LF_mid %>% 
  select(max_abund, max_abund_scale, max_first_appearance, region, subregion, duration_months) %>% 
  rename(abund = max_abund, abund_scale = max_abund_scale, date = max_first_appearance) %>% 
  mutate(time = 'max') 
LF_maxmin = rbind(LF_maxmin_max_only, LF_maxmin_min_only)
LF_maxmin$subregion = as.factor(LF_maxmin$subregion)

LF_time$time = as.factor(LF_time$time)
leg = 'Subregion'
x = ggplot(data = LF_time) +
  #geom_point(aes(x = date, y = abund, colour = subregion)) +
  #scale_colour_manual(values = c('red2', 'yellow1', 'green4')) +
  #facet_wrap(~region) +
  geom_line(aes(x = duration_months, y = abund, colour = subregion), size = 1, position = position_jitter(w=0,h=0.02)) +
  scale_color_npg() +
  theme_bw() +
  labs(title = 'Lionfish ROI', x = 'Time (Months)', y = 'Lionfish Abundance')
  
x = x + 
    geom_point(aes(x = duration_months, y = abund, fill = time), size = 3) +
  

ggplot(data = LF_maxmin) +
  #geom_point(aes(x = date, y = abund, colour = subregion)) +
  #scale_colour_manual(values = c('red2', 'yellow1', 'green4')) +
  facet_wrap(~region) +
  geom_line(aes(x = duration_months, y = abund, colour = subregion), size = 1) +
  scale_colour_viridis_d(option = 'D')


#calculate the mean rate of increase for each subreion, then region
LF_roi_sub_scale = LF_mid %>% 
  mutate(roi = (max_abund_scale - min_abund_scale)/duration_months)
LF_roi_reg_scale = LF_roi_sub %>% 
  group_by(region) %>% 
  summarize(roi = mean(roi))

LF_roi_sub = LF_mid %>% 
  filter(max_abund > 2) %>% 
  filter(duration_months > 1) %>% 
  mutate(roi = (max_abund - min_abund)/duration_months) %>% 
  mutate(roiyr = roi * 12) %>% 
  select(subregion, region, roi, roiyr, min_abund, duration_months, max_abund)
LF_roi_reg = LF_roi_sub %>% 
  group_by(region) %>% 
  summarize(roi = mean(roi)) %>% 
  mutate(roiyr = roi * 12)

temp = LF_max_only %>% 
  filter(subregion %notin% badsub) 
max(temp$duration_months)/12
min(temp$duration_months)
mean(temp$duration_months)/12

temp = LF_max_only %>% 
  filter(subregion %notin% badsub) %>% 
  filter(abund == 4)
max(temp$duration_months)/12
min(temp$duration_months)/12
mean(temp$duration_months)/12


### Update: Doing a timeline with the first highest annual average

LF_max_years = LF_all %>% 
  filter(lfabund != 0) %>% ## IMPORTANT! There are 4 regions that don't have any observations of lionfish - identify these later
  group_by(subregion, year) %>% 
  summarize(yearly_avg = mean(lfabund)) %>% 
  group_by(subregion, year) %>% 
  mutate(max = 
           
           
           
           

           
           )
