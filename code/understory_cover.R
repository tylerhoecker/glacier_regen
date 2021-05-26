library(tidyverse)

# Directories to data storage outside of Git repository
data_dir <- '/Users/sprague/Box/Work/PhD/Glacier/Data/'

# The complete 'raw' plot data table
plot_df <- read_csv(file.path(data_dir,'glac_regen_2019_2020.csv')) %>% 
  # A 'plot_id' made up of the site name (not all unique) and the date (combination is unique)
  mutate(plot_id = paste(name, today, sep = '-')) %>% 
  rownames_to_column(var = "plot_num")

cover_df <- plot_df %>% 
  # WRANGLE INTO DESIRED FORMAT
  select(plot_num, plot_id, season, starts_with('vegcover'), -contains('note')) %>% 
  pivot_longer(cols = c(starts_with('vegcover')), 
               names_to = c('temp','tran','quad','type'), 
               names_sep = '_',
               values_to = 'cover') %>% 
  select(-temp) %>% 
  # Calculate average cover by cover type for each site
  group_by(plot_num,plot_id,season,type) %>% 
  summarise(cover = mean(cover)) %>% 
  pivot_wider(names_from = 'type', values_from = 'cover') 

write_csv(cover_df, 'ftype_cover.csv')
