library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#What scenario do we want? (theoretically you only have to change this)
scen = 'biochar_ref'

#Connect to the database
conn <- localDBConn('Biochar GCAM Data', scen)

#Assign the query to a project file 
prj <- addScenario(conn, paste(scen, '_proj', sep = ''), 
                   queryFile = 'biochar_queries.xml', 
                   scenario = 'Reference', clobber = TRUE)

#Pull the detailed land allocation dataframe out of the project
detailed_land_allocation <- getQuery(prj, 'detailed land allocation')

#Pulls the irrigation by tech dataframe out of the project
irrigation_water_tech <- getQuery(prj, 'irrigation water withdrawals by ag tech')



#Separate the landleaf column from the land allocation data
detailed_land_allocation %>%
  separate_wider_delim(landleaf, delim = '_', 
                       names = c('Crop', 'Region', 'IRR', 'IRR_Level', 'Biochar', 'Biochar_Amount'),
                       too_few = 'align_start') -> sep_detailed_land_allocation


#Plot amount of biochar by crop
#Make colors
corn_colors = brewer.pal(5, 'Oranges')[3:5]
soybean_colors = brewer.pal(5, 'Greens')[3:5]
fiber_colors = brewer.pal(5, 'Purples')[3:5]
other_colors = brewer.pal(5, 'Greys')[3:5]
wheat_colors = brewer.pal(5, 'Blues')[3:5]

#Filter the only crops we care about
sep_detailed_land_allocation %>%
  drop_na() %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) -> sep_detailed_land_allocation_bio
  
#Factor the data for plotting
sep_detailed_land_allocation_bio$Crop <- factor(sep_detailed_land_allocation_bio$Crop, levels = c(
  'CornC4 5', 'CornC4 15', 'CornC4 30',
  'Soybean 5' ,'Soybean 15','Soybean 30', 
  'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
  'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
  'Wheat 5','Wheat 15','Wheat 30'
))

#Make plot not including zero biochar
sep_detailed_land_allocation_bio %>% 
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat='identity') +
  labs(title = paste('Biochar Crop Area (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Biochar~Land~(km^2%*%10^3))) +
  scale_fill_manual(name = "Data Source", 
                    breaks = c(
                      'CornC4 5', 'CornC4 15', 'CornC4 30',
                      'Soybean 5' ,'Soybean 15','Soybean 30', 
                      'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
                      'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
                      'Wheat 5','Wheat 15','Wheat 30'
                    ),
                    values = c(corn_colors, soybean_colors, fiber_colors, other_colors, wheat_colors)) +
  ylim(0, 700) + 
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, '_only_biochar_crop.jpeg', sep = ''), path = 'Graphs')



#Plot ALL crops with biochar amount (including zero)
#Make colors
corn_colors = brewer.pal(5, 'Oranges')[2:5]
soybean_colors = brewer.pal(5, 'Greens')[2:5]
fiber_colors = brewer.pal(5, 'Purples')[2:5]
other_colors = brewer.pal(5, 'Greys')[2:5]
wheat_colors = brewer.pal(5, 'Blues')[2:5]

#Filter data
sep_detailed_land_allocation %>%
  mutate(Biochar_Amount = as.double(Biochar_Amount)) %>%
  replace_na(list(Biochar_Amount = 0)) %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) -> sep_detailed_land_allocation_all

#Factor data for plotting
sep_detailed_land_allocation_all$Crop <- factor(sep_detailed_land_allocation_all$Crop, levels = c(
  'CornC4 0','CornC4 5', 'CornC4 15', 'CornC4 30',
  'Soybean 0', 'Soybean 5' ,'Soybean 15','Soybean 30', 
  'FiberCrop 0', 'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
  'OtherGrainC4 0', 'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
  'Wheat 0', 'Wheat 5','Wheat 15','Wheat 30'
))

#Plot data with zero biochar
sep_detailed_land_allocation_all %>%
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat= 'identity') +
  labs(title = paste('Total Crop Area (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Biochar~Land~(km^2%*%10^3))) +
  scale_fill_manual(name = "Data Source", 
                    breaks = c(
                      'CornC4 0','CornC4 5', 'CornC4 15', 'CornC4 30',
                      'Soybean 0', 'Soybean 5' ,'Soybean 15','Soybean 30', 
                      'FiberCrop 0', 'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
                      'OtherGrainC4 0', 'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
                      'Wheat 0', 'Wheat 5','Wheat 15','Wheat 30'
                    ),
                    values = c(corn_colors, soybean_colors, fiber_colors, other_colors, wheat_colors)) +
  ylim(0, 7000) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, '_total_crop_area.jpeg', sep = ''), path = 'Graphs')



#Separate the tech column from the water use data
irrigation_water_tech %>%
  separate_wider_delim(technology, delim = '_', 
                       names = c('Crop', 'Region', 'IRR', 'IRR_Level', 'Biochar', 'Biochar_Amount'),
                       too_few = 'align_start') -> sep_irrigation_water_tech


#Plot amount of irrigation water withdrawals by crop
#Make colors
corn_colors = brewer.pal(5, 'Oranges')[3:5]
soybean_colors = brewer.pal(5, 'Greens')[3:5]
fiber_colors = brewer.pal(5, 'Purples')[3:5]
other_colors = brewer.pal(5, 'Greys')[3:5]
wheat_colors = brewer.pal(5, 'Blues')[3:5]

#Filter the only crops we care about
sep_irrigation_water_tech %>%
  drop_na() %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) -> sep_irrigation_water_tech_bio

#Factor the data for plotting
sep_irrigation_water_tech_bio$Crop <- factor(sep_irrigation_water_tech_bio$Crop, levels = c(
  'CornC4 5', 'CornC4 15', 'CornC4 30',
  'Soybean 5' ,'Soybean 15','Soybean 30', 
  'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
  'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
  'Wheat 5','Wheat 15','Wheat 30'
))

#Make plot not including zero biochar
sep_irrigation_water_tech_bio %>% 
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat='identity') +
  labs(title = paste('Irrigation Water Withdrawal (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Water~Withdrawal~(km^3))) +
  scale_fill_manual(name = "Data Source", 
                    breaks = c(
                      'CornC4 5', 'CornC4 15', 'CornC4 30',
                      'Soybean 5' ,'Soybean 15','Soybean 30', 
                      'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
                      'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
                      'Wheat 5','Wheat 15','Wheat 30'
                    ),
                    values = c(corn_colors, soybean_colors, fiber_colors, other_colors, wheat_colors)) +
  ylim(0, 80)
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, '_only_biochar_water.jpeg', sep = ''), path = 'Graphs')



#Plot ALL water use with biochar amount (including zero)
#Make colors
corn_colors = brewer.pal(5, 'Oranges')[2:5]
soybean_colors = brewer.pal(5, 'Greens')[2:5]
fiber_colors = brewer.pal(5, 'Purples')[2:5]
other_colors = brewer.pal(5, 'Greys')[2:5]
wheat_colors = brewer.pal(5, 'Blues')[2:5]

#Filter data
sep_irrigation_water_tech %>%
  mutate(Biochar_Amount = as.double(Biochar_Amount)) %>%
  replace_na(list(Biochar_Amount = 0)) %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) -> sep_irrigation_water_tech_all

#Factor data for plotting
sep_irrigation_water_tech_all$Crop <- factor(sep_irrigation_water_tech_all$Crop, levels = c(
  'CornC4 0','CornC4 5', 'CornC4 15', 'CornC4 30',
  'Soybean 0', 'Soybean 5' ,'Soybean 15','Soybean 30', 
  'FiberCrop 0', 'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
  'OtherGrainC4 0', 'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
  'Wheat 0', 'Wheat 5','Wheat 15','Wheat 30'
))


#Plot water data with zero biochar
sep_irrigation_water_tech_all %>%
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat= 'identity') +
  labs(title = paste('Irrigation Water Withdrawal (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Water~Withdrawal~(km^3))) +
  scale_fill_manual(name = "Data Source", 
                    breaks = c(
                      'CornC4 0','CornC4 5', 'CornC4 15', 'CornC4 30',
                      'Soybean 0', 'Soybean 5' ,'Soybean 15','Soybean 30', 
                      'FiberCrop 0', 'FiberCrop 5', 'FiberCrop 15', 'FiberCrop 30', 
                      'OtherGrainC4 0', 'OtherGrainC4 5', 'OtherGrainC4 15', 'OtherGrainC4 30', 
                      'Wheat 0', 'Wheat 5','Wheat 15','Wheat 30'
                    ),
                    values = c(corn_colors, soybean_colors, fiber_colors, other_colors, wheat_colors)) +
  ylim(0,1250) + 
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, '_total_water.jpeg', sep = ''), path = 'Graphs')


  
  