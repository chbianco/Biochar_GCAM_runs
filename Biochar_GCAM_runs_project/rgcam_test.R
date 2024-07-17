library(rgcam)
library(ggplot2)
library(dplyr)
library(tidyr)

#What scenario do we want? (theoretically you only have to change this)
scen = 'biochar_ref'

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

#Plot the amount of biochar used 
sep_detailed_land_allocation %>%
  drop_na() %>%
  filter(year > 2015) %>%
  ggplot(aes(x = year, y = value, fill = Biochar_Amount)) +
  geom_bar(stat='identity') +
  labs(title = paste('Total Biochar Land (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Biochar~Land~(km^2%*%10^3))) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, 'total_biochar_land.jpeg', sep = ''), path = 'Graphs')


#Plot amount of biochar by crop
sep_detailed_land_allocation %>%
  drop_na() %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) %>%
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat='identity') +
  labs(title = paste('Biochar Crop Area (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Biochar~Land~(km^2%*%10^3))) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, 'only_biochar_crop.jpeg', sep = ''), path = 'Graphs')

#Plot ALL crops with biochar amount (including zero)
sep_detailed_land_allocation %>%
  mutate(Biochar_Amount = as.double(Biochar_Amount)) %>%
  replace_na(list(Biochar_Amount = 0)) %>%
  filter(year > 2015) %>%
  filter(
    (Crop == 'CornC4') | (Crop == 'Soybean') | (Crop == 'FiberCrop') | (Crop == 'OtherGrainC4') | (Crop == 'Wheat')) %>%
  mutate(Crop = paste(Crop, Biochar_Amount, sep = ' ')) %>%
  ggplot(aes(x = year, y = value, fill = Crop)) +
  geom_bar(stat='identity') +
  labs(title = paste('Total Crop Area (', scen, ')', sep = '' ), x = "Year", y = expression(Total~Biochar~Land~(km^2%*%10^3))) +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave(paste(scen, 'total_crop_area.jpeg', sep = ''), path = 'Graphs')


  
  