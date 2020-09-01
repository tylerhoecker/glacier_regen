library(tidyverse)

plot_df <- read_csv('data/glac_plot_data_2019.csv') 

postfire_df <- plot_df %>% 
  dplyr::select(starts_with('plot'), starts_with('seedlings'), phys.phys_elev, phys.phys_aspect,
                -contains('header'), -contains('other'), -contains('end')) %>% 
  gather(key, value, -contains('length'), -starts_with('plot'), -starts_with('phys')) %>% 
  separate(key, into = c('del1','del2','del3','del4','transect','year','sp'), sep = "_") %>% 
  dplyr::select(-starts_with('del'), length_t1 = seedlings_t1.seedlings_t1_length, length_t2 = seedlings_t2.seedlings_t2_length) %>%
  mutate_at(vars(length_t1, length_t2), ~ ifelse(is.na(.), 28, .)) %>% 
  mutate(seedling_count = if_else(is.na(value), 0, value)) %>% 
  # Temporarily filter out the 6 (of 2093) trees we forgot to record the species of... 
  # Condense some species into groups
  mutate(sp = fct_collapse(sp, DECID = c('BEPA','ACGL','ACOC'), CDHM = c('TSHE','THPL'), SPFR = c('ABGR','PIEN','ABLA') )) %>% 
  mutate(length = length_t1 + length_t2) %>% 
  group_by(plot.utm_easting, plot.utm_northing, phys.phys_aspect, plot.name, phys.phys_elev, year, sp, length) %>%
  summarise(seedling_count = sum(seedling_count)) %>% 
  mutate(area = length * 2,
         seed_dens = seedling_count / area,
         dev_ne = cos( (45*pi/180) - (phys.phys_aspect*pi/180) ) + 1) %>% 
  ungroup() %>% 
  mutate(plot.name = fct_reorder(plot.name, phys.phys_elev)) %>% 
  mutate(age = if_else(year == 'age1', 1, 2)) %>% 
  select(-year, -length) %>% 
  rename(easting = plot.utm_easting,
         northing = plot.utm_northing,
         plot_id = plot.name,
         elev = phys.phys_elev,
         stem_count = seedling_count,
         stem_dens = seed_dens)


postfire_df %>% 
  group_by(sp) %>% 
  summarise(mean_sp = mean(stem_dens, na.rm = T)*10000)

lm(stem_dens ~ dev_ne*sp, data = filter(postfire_df, stem_dens < 25)) %>% 
  summary()

postfire_df %>% 
  filter(stem_dens < 25) %>% 
  ggplot(., aes(x = elev, y = stem_dens)) +
    geom_point(size = 2, alpha = 0.4) +
    geom_smooth(se = F) +
    ylim(0,25) +
    #coord_cartesian(ylim = c(0,5)) +
    scale_color_brewer(palette = 'Dark2') +
    theme_bw(base_size = 12) 



ggplot(postfire_df) +
  geom_col(aes(x = plot_id, y = stem_dens, fill = sp)) +
  #coord_cartesian(ylim = c(0,10)) +
  scale_x_discrete(labels = sort(unique(postfire_df$elev))) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_bw(base_size = 12) 
