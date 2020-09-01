library(tidyverse)

plot_df <- read_csv('data/glac_plot_data_2019.csv') 

prefire_df <- plot_df %>% 
  dplyr::select(starts_with('plot'), starts_with('prefire'), phys.phys_elev) %>% 
  gather(key, value, -prefire_n_quads, -starts_with('plot'), -starts_with('phys')) %>% 
  separate(key, into = c('del1','del2','del3','del4','quad','tree','variable','other'), sep = "_") %>% 
  dplyr::select(-del1, -del2, -del3, -del4, other, -starts_with('plot.plot_'), -starts_with('plot.utm_')) %>%
  filter(!is.na(value)) %>% 
  # Must merge 'other' species, which are stored as separate rows
  spread(., variable, value) %>%
  mutate_at(vars(tree,cones,dbh,serotinous), as.numeric) %>% 
  group_by(plot.name, phys.phys_elev, prefire_n_quads, quad, tree) %>% 
  summarise(sp = as.factor(sp[1]),
            cones = max(cones, na.rm = T),
            dbh = max(dbh, na.rm = T),
            serotinous = max(serotinous, na.rm = T)) %>% 
  # Just tidy artifacts of finding max of two NA's, enforce no serotiny w/o cones
  mutate(cones = ifelse(cones == -Inf, 0, 
                        ifelse(is.na(cones), 0, cones))) %>% 
  mutate(serotinous = ifelse(cones == 0, NA,
                             ifelse(cones == 1 & serotinous == -Inf, 0,
                                    ifelse(cones == -Inf, 0, serotinous)))) %>% 
  # Temporarily filter out the 6 (of 2093) trees we forgot to record the species of... 
  # Condense some species into groups
  mutate(sp = fct_collapse(sp, DECID = c('BEPA','ACGL','ACOC'), CDHM = c('TSHE','THPL'), SPFR = c('ABGR','PIEN','ABLA') )) %>% 
  filter(!is.na(sp))


prefire_stats <- prefire_df %>% 
  # Calculate some actual statistics!
  mutate(basal = (pi * (dbh/2)^2)/1000000) %>%  # cross-sectional area in m2 (dbh was measured in mm)
  group_by(plot.name, phys.phys_elev, sp) %>% 
  summarise(stems = n(),
            area = (max(prefire_n_quads) * (0.07/4)), # area in ha
            density = (stems/area),
            serotiny = sum(serotinous, na.rm = T)/sum(cones),
            mean_dbh = mean(dbh),
            basal = mean(basal)/area) %>% 
  ungroup() %>% 
  #view()
  mutate(plot.name = fct_reorder(plot.name, phys.phys_elev),
         elev_class = as.factor(round(phys.phys_elev/100))) %>% 
  #group_by(plot.name) %>%
  group_by(elev_class) %>%
  mutate(site_basal = sum(basal)/max(area),
         site_stems = sum(stems),
         site_area = max(area),
         site_density = sum(density)) %>%
  #group_by(plot.name, sp) %>%
  group_by(elev_class, sp) %>% 
  mutate(impval = (stems/site_stems) + (basal/site_basal)) %>%
  group_by(plot.name) %>% 
  top_n(1, impval) 
  

# Colors
sp_cols <- c('LAOC' = '#1b9e77', 'CDHM' = '#d95f02', 'DECID' = '#7570b3',
             'PSME' = '#e7298a', 'PIMO' = '#66a61e', 'SPFR' = '#e6ab02','PICO' = '#a6761d')

ggplot(prefire_stats) +
  geom_bar(aes(x = elev_class, fill = sp)) +
  scale_x_discrete(labels = sort(unique(round(prefire_df$phys.phys_elev/100)*100))) +
  #scale_x_discrete(labels = sort(unique(prefire_df$phys.phys_elev))) +
  scale_fill_manual(values = sp_cols) +
  #scale_y_continuous(labels = c(0,1,2), breaks = c(0,1,2)) +
  theme_bw(base_size = 12) +
  labs(x = 'Plot elevation (m)', y = '')



prefire_df %>% 
  # Calculate some actual statistics!
  mutate(basal = (pi * (dbh/2)^2)/1000000) %>%  # cross-sectional area in m2 (dbh was measured in mm)
  group_by(plot.name, phys.phys_elev, sp) %>% 
  summarise(stems = n(),
            area = (max(prefire_n_quads) * (0.07/4)), # area in ha
            density = (stems/area),
            serotiny = sum(serotinous, na.rm = T)/sum(cones),
            mean_dbh = mean(dbh),
            basal = mean(basal)/area) %>% 
  ungroup() %>% 
  #view()
  mutate(plot.name = fct_reorder(plot.name, phys.phys_elev),
         elev_class = as.factor(round(phys.phys_elev/100))) %>% 
  ggplot() +
    geom_bar(aes(x = elev_class, fill = sp)) +
    scale_x_discrete(labels = sort(unique(round(prefire_df$phys.phys_elev/100)*100))) +
    #scale_x_discrete(labels = sort(unique(prefire_df$phys.phys_elev))) +
    scale_fill_manual(values = sp_cols) +
    #scale_y_continuous(labels = c(0,1,2), breaks = c(0,1,2)) +
    theme_bw(base_size = 12) +
    labs(x = 'Plot elevation (m)', y = '')


# %>% 
#   group_by(name, plot_fire,plot_elevation_cat,plot_aspect_cat, plot_direction, tree, variable, value) %>% 
#   summarise(ha = length(unique(quadrant)) * (0.07/4)) %>% 
#   group_by(name, plot_fire,plot_elevation_cat,plot_aspect_cat, plot_direction, variable) 


  
