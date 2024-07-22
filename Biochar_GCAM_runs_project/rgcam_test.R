library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#What scenario do we want? (theoretically you only have to change this)
scen = 'biochar_carbon_price_100'

#Connect to the database
conn <- localDBConn('Biochar GCAM Data', scen)

#Assign the query to a project file 
prj <- addScenario(conn, paste(scen, '_proj', sep = ''), 
                   queryFile = 'biochar_queries.xml', 
                   scenario = 'Reference', clobber = TRUE)

#Pull the land allocation dataframe out of the project
land_allocation <- getQuery(prj, 'aggregated land allocation')
detailed_land_allocation <- getQuery(prj, 'detailed land allocation')

#Separate the landleaf column
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


  
  