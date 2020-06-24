library(tidyverse)
library(lubridate)
library(ggsci)
library(RColorBrewer)
library(data.table)

#pull data on range-restricted species
restr_spp = read_csv(here('./data/restricted-range-spp-by-country.csv'))
subr_names = read_csv(here('./data/REEF_subregions_by_name.csv'))
reef_effort = read_csv(here('./data/REEF_effort_data_fromMaster.csv'), 
                       guess_max = 1000000)

#restr_spp = restr_spp[1:36,1:3]
#replace the non-parsed accented ç
restr_spp[8,1] = 'Curacao'
restr_spp[15,1] = 'St. Eustatius'
restr_spp[28,1] = 'St. Barthelemy'

#match number of surveys for the reef data
#recall that reef_effort is the dataframe that has all the info for each survey
names(reef_effort)
sort(unique(reef_effort$subregion))


#make additional column for the first 4 numbers of the geogr code
reef_effort$four_geogr = substr(reef_effort$geogr, 0,4)
reef_effort$Country = NA


#R gods please forgive me for I have sinned in writing this hideous loop



for(row in 1:nrow(reef_effort)) {
  temp = reef_effort[row,]
  reef_effort$Country[row] = ifelse(temp$region == 3, 
           'United States',
           ifelse(temp$subregion %in% c(21,22,23,24),
                  'United States', 
                  ifelse(temp$subregion %in% c(25,26,27,28,29,54),
                         'Mexico', 
                         ifelse(temp$subregion %in% c(41,42,43),
                                'The Bahamas', 
                                ifelse(temp$subregion == 44,
                                       'Turks and Caicos',
                                       ifelse(temp$subregion == 51,
                                              'Cuba',
                                              ifelse(temp$subregion == 52,
                                                     'Cayman Islands',
                                                            ifelse(temp$subregion == 55,
                                                                   'Belize',
                                                                   ifelse(temp$subregion == 56,
                                                                          'Guatemala',
     ifelse(temp$subregion == 57,
            'Honduras',
            ifelse(temp$subregion == 58,
                   'Nicaragua',
                   ifelse(temp$subregion == 61,
                          'Haiti',
                          ifelse(temp$subregion == 62,
                                 'Dominican Republic',
                                 ifelse(temp$subregion == 63,
                                        'Puerto Rico',
                                 ifelse(temp$subregion == 64,
                                        'U.S. Virgin Islands',
                                        ifelse(temp$subregion == 65,
                                               'British Virgin Islands',
                                               ifelse(temp$subregion == 81,
                                                      'Costa Rica',
                                                      ifelse(temp$subregion == 82,
                                                             'Panama',
                                                             ifelse(temp$subregion == 83,
                                                                    'Colombia',
                                                                    ifelse(temp$subregion == 84,
                                                                           'Venezuela',
     ifelse(temp$subregion == 86,
            'Trinidad and Tobago',
            ifelse(temp$four_geogr == 1101,
                   'Bermuda',
                   ifelse(temp$four_geogr %in% c(5303,5302,5305,5307,5301),
                          'Jamaica',
                   ifelse(temp$four_geogr == 5306,
                          'Navassa Island',
                          ifelse(temp$four_geogr == 7103,
                                 'St. Barthelemy',
                                 ifelse(temp$four_geogr == 7105,
                                        'St. Eustatius',
                                        ifelse(temp$four_geogr %in% c(7107,7306),
                                               'Saint Kitts and Nevis',
                                               ifelse(temp$four_geogr == 7112,
                                                      'Dominica',
                                                      ifelse(temp$four_geogr == 7301,
                                                             'Martinique',
                                                             ifelse(temp$four_geogr == 7302,
                                                                    'St. Lucia',
                                                                    ifelse(temp$four_geogr %in% c(7303,7306),
                                                                           'St. Vincent and the Grenadines',
                                                                           ifelse(temp$four_geogr == 7304,
                                                                                  'Barbados',
      ifelse(temp$four_geogr == 8501,
             'Aruba',
             ifelse(temp$four_geogr == 8502,
                    'Curacao',
                    ifelse(temp$four_geogr == 8503,
                           'Bonaire',
                           ifelse(temp$geogr %in% c(71040022,71040023,71040024),
                                  'Saba Bank',NA))))))))))))))))))))))))))))))))))))
}
#note that Guatemala has no reef data associated with it

#grab the surveys and summarize by the subregion numbers
reef_effort_all = reef_effort

no_obs_subr_all = reef_effort_all %>% 
  group_by(Country) %>% 
  summarize(obs = n())

#add in Guatemala with a zero since that's still data
no_obs_subr_all[35,1] = 'Guatemala'
no_obs_subr_all[35,2] = 0

# for(i in 1:nrow(no_obs_subr)) {
#   sub = no_obs_subr[i,1]
#   sub = as.integer(sub)
#   name = subr_names %>% filter(Subregion == sub)
#   no_obs_subr[i,1] = name[1,2]
# }
# no_obs_subr = no_obs_subr %>% 
#   group_by(subregion) %>% 
#   summarize(obs = sum(obs))
# no_obs_subr = no_obs_subr %>% 
#   rename(Country = subregion)
rest_spp = left_join(no_obs_subr_all, restr_spp, by = "Country")

reg_plot = ggplot(data = rest_spp) +
  geom_point(aes(x = obs, y = shallow, colour = 'red2'))+
  geom_smooth(aes(x = obs, y = shallow), method = 'lm', formula = y ~ x, colour = 'red') +
  geom_point(aes(x = obs, y = deep, colour = 'blue1'))+
  geom_smooth(aes(x = obs, y = deep), method = 'lm', formula = y ~ x, colour = 'blue1')+
  scale_colour_discrete('Species Depth', labels = c('shallow', 'deep'))+
  labs(x = 'Number of Surveys', y = '# Range Restricted Spp')+
  theme_classic()

reg_plot_log = ggplot(data = rest_spp) +
  geom_point(aes(x = log(obs), y = shallow, colour = 'red2'))+
  geom_smooth(aes(x = log(obs), y = shallow), method = 'lm', formula = y ~ x, colour = 'red') +
  geom_point(aes(x = log(obs), y = deep, colour = 'blue1'))+
  geom_smooth(aes(x = log(obs), y = deep), method = 'lm', formula = y ~ x, colour = 'blue1')+
  scale_colour_discrete('Depth', labels = c('Shallow (<40m)', 'Deep (>=40m)'))+
  labs(x = 'Number of Surveys (Log)', y = '# Range Restricted Spp') +
  theme_classic()

#try with just expert data
reef_effort_e = reef_effort_all %>% 
  filter(exp == 'E')
no_obs_subr_e = reef_effort_e %>% 
  group_by(Country) %>% 
  summarize(obs = n())
#add in zeros since that's still data (guatemala, costa rica, nicaragua)
no_obs_subr_e[33,1] = 'Guatemala'
no_obs_subr_e[33,2] = 0
no_obs_subr_e[34,1] = 'Nicaragua'
no_obs_subr_e[34,2] = 0
no_obs_subr_e[35,1] = 'Costa Rica'
no_obs_subr_e[35,2] = 0

rest_spp_e = left_join(no_obs_subr_e, restr_spp, by = "Country")

# no_obs_subr = reef_effort_expert %>% 
#   group_by(subregion) %>% 
#   summarize(obs = n())
# for(i in 1:nrow(no_obs_subr)) {
#   sub = no_obs_subr[i,1]
#   sub = as.integer(sub)
#   name = subr_names %>% filter(Subregion == sub)
#   no_obs_subr[i,1] = name[1,2]
# }
# no_obs_subr = no_obs_subr %>% 
#   group_by(subregion) %>% 
#   summarize(obs = sum(obs))
# no_obs_subr = no_obs_subr %>% 
#   rename(Country = subregion)
# rest_spp = left_join(no_obs_subr, restr_spp, by = "Country")

# reg_plot_e = ggplot(data = rest_spp_e) +
#   geom_point(aes(x = obs, y = shallow, colour = 'red2'))+
#   geom_smooth(aes(x = obs, y = shallow), method = 'lm', formula = y ~ x, colour = 'red') +
#   geom_point(aes(x = obs, y = deep, colour = 'blue1'))+
#   geom_smooth(aes(x = obs, y = deep), method = 'lm', formula = y ~ x, colour = 'blue1')+
#   scale_colour_discrete('Species Depth', labels = c('shallow', 'deep'))+
#   labs(x = 'Number of Surveys', y = '# Range Restricted Spp')+
#   theme_classic()
# 
# reg_plot_log_e = ggplot(data = rest_spp_e) +
#   geom_point(aes(x = log(obs), y = shallow, colour = 'red2'))+
#   geom_smooth(aes(x = log(obs), y = shallow), method = 'lm', formula = y ~ x, colour = 'red') +
#   geom_point(aes(x = log(obs), y = deep, colour = 'blue1'))+
#   geom_smooth(aes(x = log(obs), y = deep), method = 'lm', formula = y ~ x, colour = 'blue1')+
#   scale_colour_discrete('Species Depth', labels = c('shallow', 'deep'))+
#   labs(x = 'Number of Surveys (Log)', y = '# Range Restricted Spp') +
#   theme_classic()
# 
# no_obs_subr = reef_effort_expert %>% 
#   group_by(subregion) %>% 
#   summarize(obs = n())
# for(i in 1:nrow(no_obs_subr)) {
#   sub = no_obs_subr[i,1]
#   sub = as.integer(sub)
#   name = subr_names %>% filter(Subregion == sub)
#   no_obs_subr[i,1] = name[1,2]
# }
# no_obs_subr = no_obs_subr %>% 
#   group_by(subregion) %>% 
#   summarize(obs = sum(obs))
# no_obs_subr = no_obs_subr %>% 
#   rename(Country = subregion)
# rest_spp = left_join(no_obs_subr, restr_spp, by = "Country")
# 
# reg_plot = ggplot(data = rest_spp) +
#   geom_point(aes(x = obs, y = shallow, colour = 'red2'))+
#   geom_smooth(aes(x = obs, y = shallow), method = 'lm', 
#               formula = y ~ x, colour = 'red') +
#   geom_point(aes(x = obs, y = deep, colour = 'blue1'))+
#   geom_smooth(aes(x = obs, y = deep), method = 'lm', 
#               formula = y ~ x, colour = 'blue1')+
#   scale_colour_discrete('Species Depth', labels = c('shallow', 'deep'))+
#   labs(x = 'Number of Surveys', y = '# Range Restricted Spp')+
#   theme_classic()

reg_plot_log = ggplot(data = rest_spp[1:33,]) +
  geom_point(aes(x = log(obs), y = shallow, fill = '#bb0a1e'), 
             shape = 21,  size = 3)+
  geom_smooth(aes(x = log(obs), y = shallow), method = 'lm', fill = '#bb0a1e', 
              formula = y ~ x, colour = '#bb0a1e', size = 1.5, alpha = 0.2) +
  geom_point(aes(x = log(obs), y = deep, fill = '#87ceeb'), 
             shape = 21,  size = 3)+
  geom_smooth(aes(x = log(obs), y = deep), method = 'lm', fill = '#87ceeb',
              formula = y ~ x, colour = '#87ceeb', size = 1.5, alpha = 0.4)+
  scale_fill_discrete('Depth', labels = c('Shallow (<40m)', 'Deep (>= 40m)'))+
  labs(x = 'Number of surveys (log scale)', 
       y = 'Number of range restricted species') +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = 'black'),  # remove x-axis text
        axis.text.y = element_text(size=14, colour = 'black'), # remove y-axis text
        axis.ticks = element_line(colour = 'black'),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        legend.title = element_text(size = 18), 
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.justification = c(0,0.5),
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        #axis.line.x.bottom = element_line(size = 1),
        #axis.line.x.top = element_line(size = 1),
        #axis.line.y.right = element_line(size = 1),
        #axis.line.y.left = element_line(size = 1),
        panel.border = element_rect(colour = 'black', size = 1.1)) +
  scale_x_continuous(limits = c(1,10), breaks = c(2,4,6,8,10))

ggsave(here('./figures/log_obs_plot.png'), plot = reg_plot_log,
       device = 'png',
       units = 'in',
       dpi = 1200, height = 8, width = 9)
ggsave(here('./figures/log_obs_plot_small.png'), plot = reg_plot_log,
       device = 'png',
       units = 'in',
       dpi = 300, height = 8, width = 9)
#try with novice surveys as well as include the zeroes as well 
#tab called REEF_geography key
#make sure the names are matching up properly with Christi's division

#timeline for getting things done:
  #Feb 03 get the figures and captions into the manuscript
  #Feb 07 chat with Christi & Steph 
  #9am MT meeting time  on Feb 07

#NOTE: couldn't find any data in REEF for French Guiana
