plot_df <- read_csv('data/glac_plot_data_2019.csv') 

postfire_df <- plot_df %>% 
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

init_seedlings <- postfire_df %>% 
  mutate(age = if_else(year == 'age1', 1, 2),
         plot.name = as.factor(plot.name),
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
  select(easting, northing, stand_id) %>% 
  arrange(stand_id)
