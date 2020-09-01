library(tidyverse)
setwd("~/GitHub/glacier_regen/")

hoecker_df <- read_csv('data/glac_plot_data_2019.csv') 

hoecker_df <- hoecker_df %>% 
  dplyr::select(starts_with('plot'), starts_with('seedlings'), phys.phys_elev, 
                -contains('header'), -contains('other'), -contains('end')) %>% 
  gather(key, value, -contains('length'), -starts_with('plot'), -starts_with('phys')) %>% 
  separate(key, into = c('del1','del2','del3','del4','transect','year','sp'), sep = "_") %>% 
  dplyr::select(-starts_with('del'), length_t1 = seedlings_t1.seedlings_t1_length, length_t2 = seedlings_t2.seedlings_t2_length) %>%
  mutate_at(vars(length_t1, length_t2), ~ ifelse(is.na(.), 28, .)) %>% 
  mutate(seedling_count = if_else(is.na(value), 0, value)) %>% 
  mutate(length = length_t1 + length_t2) %>% 
  group_by(plot.utm_easting, plot.utm_northing, plot.name, phys.phys_elev, year, sp, length) %>%
  summarise(seedling_count = sum(seedling_count)) %>% 
  mutate(area_m2 = length * 2,
         seed_dens_ha = (seedling_count / area_m2) * 10000) %>% 
  ungroup()

hoecker_full <- hoecker_df %>% 
  mutate(age = if_else(year == 'age1', 1, 2),
         plot_name = paste0('TH-',as.factor(plot.name)),
         stand_id = as.numeric(as.factor(plot.name))) %>% 
  select(-plot.name, -phys.phys_elev, -year, -length, -seedling_count, -area_m2) %>% 
  rename(easting = plot.utm_easting,
         northing = plot.utm_northing,
         species = sp,
         count = seed_dens_ha) %>% 
  group_by(stand_id, species, age) %>% 
  filter(count == max(count), count > 0) %>% 
  mutate(count = round(count))  %>% 
  mutate(height_from = if_else(age == 1, .02, 0.08),
         height_to = if_else(age == 1, 0.05, 0.15)) %>% 
  select(plot_name, easting, northing, stand_id, species, count, age, height_from, height_to) %>% 
  arrange(stand_id, species, age) %>% 
  ungroup() 

hoecker_full 

mckenzie_df <- read_csv('data/mckenzie_glac.csv')

mckenzie_temp <- mckenzie_df %>% 
  filter(count > 0) %>% 
  mutate(count_ha = round((count / (50 * width)) * 10000)) %>% 
  group_by(fire, plot, species) %>% 
  filter(count_ha == max(count_ha),
         #fire == 'RE06',
         !species %in% c('ACGL','ALVI','JUCO','POTR','POBA','TABR','BEPA')) %>% 
  arrange(fire, plot, species) %>% 
  group_by(fire, plot) %>% 
  mutate(stand_id = max(hoecker_full$stand_id) + group_indices(),
         age = if_else(fire == 'RE06', 1, 3),
         height_from = if_else(age == 1, 0.02, 0.1),
         height_to = if_else(age == 1, 0.05, 0.3)) %>% 
  ungroup() %>% 
  arrange(stand_id, species) %>% 
  mutate(plot_name = toupper(paste0(fire,plot))) %>% 
  select(plot_name, stand_id, species, count = count_ha, age, height_from, height_to)

mckenzie_pts <- read_csv('data/mckenzie_plots.csv') %>% 
  mutate(plot_name = str_sub(plot_name, end = -2)) %>% 
  group_by(plot_name) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(easting = round(easting),
         northing = round(northing)) 

mckenzie_full <- mckenzie_temp %>% 
  full_join(mckenzie_pts, by = 'plot_name') %>% 
  mutate(plot_name = paste0('DM-',plot_name)) %>% 
  select(plot_name, easting, northing, stand_id, species, count, age, height_from, height_to)


seedlings_full <- rbind(hoecker_full, mckenzie_full) %>% 
  # A little confusing... but this is changing the name of stand_id to env_id (the key to link to climate files)
  # ...then making a new 'stand_id' column that is unique to each row, which is what iLand will need
  mutate(env_id = stand_id,
         stand_id = 1:nrow(.)) 


seedlings_full %>% 
  select(env_id, stand_id, easting, northing, species, plot_name) %>% 
  write_csv('seedling_init_key.txt')


seedlings_full %>% 
  select(stand_id, species, count, height_from, height_to, age) %>% 
  write_csv('seedling_init.txt')







# OLD
  # spread(age, count) %>% 
  # group_by(stand_id, species) %>% 
  # summarise(`1` = max(`1`, na.rm = T),
  #           `2` = max(`2`, na.rm = T)) %>% 
  # group_by(stand_id) %>% 
  # filter(`1` == max(`1`) | `2` == max(`2`)) %>% 
  # gather(age, count, `1`,`2`) %>% 
  # arrange(stand_id, species, age) %>% 
  # filter(count > 0) %>% 
  # mutate(count = round(count))  %>% 
  # mutate(height_from = if_else(age == 1, .02, 0.08),
  #        height_to = if_else(age == 1, 0.05, 0.15)) %>% 
  # select(stand_id, species, count, age, height_from, height_to) %>% 
  # ungroup() 

