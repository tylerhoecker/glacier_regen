library(tidyverse)


plot_df <- read_csv('data/glac_plot_data_2019.csv') 

postfire_df <- plot_df %>% 
  dplyr::select(starts_with('plot'), starts_with('seedlings'), phys.phys_elev, 
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
  group_by(plot.name, phys.phys_elev, sp, length) %>%
  summarise(seedling_count = sum(seedling_count)) %>% 
  mutate(area_m2 = length * 2,
         seed_dens_ha = (seedling_count / area_m2) * 10000) %>% 
  ungroup() %>% 
  mutate(plot.name = fct_reorder(plot.name, phys.phys_elev))


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
  filter(!is.na(sp)) %>% 
  # Calculate some metrics
  mutate(basal = (pi * dbh^2 / 40000)) %>% 
  group_by(plot.name, phys.phys_elev, sp) %>% 
  summarise(stems = n(),
            area_ha = (max(prefire_n_quads) * (0.07/4)),
            density = (stems/area_ha),
            serotiny = sum(serotinous, na.rm = T)/sum(cones),
            mean_dbh = mean(dbh),
            basal = sum(basal)) %>% 
  ungroup() %>% 
  mutate(plot.name = fct_reorder(plot.name, phys.phys_elev),
         elev_class = as.factor(round(phys.phys_elev/100))) %>% 
  group_by(plot.name) %>%
  #group_by(elev_class) %>%
  mutate(site_basal = sum(basal),
         site_stems = sum(stems),
         site_area = sum(area_ha),
         site_density = sum(density)) %>%
  group_by(plot.name, sp) %>%
  mutate(impval = (stems/site_stems) + (basal/site_basal)) 

# %>%
#   group_by(elev_class, sp) %>% 
#   mutate(impval = (stems/site_stems) + (basal/site_basal))


combined_df <- 
  full_join(prefire_df, postfire_df) %>% 
  filter(seed_dens_ha > 0) 

pre_dom <- prefire_df %>% 
  ungroup() %>% 
  select(plot.name, elev_class, sp, density, impval) %>% 
  group_by(plot.name) %>% 
  top_n(1, impval) %>% 
  rename(pre_sp = sp, pre_dens = density)

post_dom <- postfire_df %>% 
  select(plot.name, sp, seed_dens_ha) %>% 
  group_by(plot.name) %>% 
  top_n(1, seed_dens_ha) %>% 
  rename(post_sp = sp, post_dens = seed_dens_ha)

comb_dom <- full_join(pre_dom, post_dom) %>% 
  gather(time, species, pre_sp, post_sp) %>%
  mutate(time = factor(time, levels = c('pre_sp', 'post_sp'))) %>%
  arrange(plot.name)

library(ggalluvial)

dom_long <- full_join(pre_dom, post_dom)

ggplot(dom_long, 
       aes(axis1 = pre_sp, axis2 = post_sp)) +
  geom_alluvium(aes(fill = pre_sp), width = 1/12) +
  geom_stratum(width = 1/12, alpha = 0.1) +
  #geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Prefire type", "Postfire type"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "Forest type", palette = "Set1") +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


ggplot(comb_dom,
       aes(x = time, stratum = species, alluvium = plot.name,
           fill = species, label = species)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow() +
  geom_alluvium() +
  geom_stratum()



ggplot(combined_df, aes(x = density, y = log(seed_dens_ha))) +
  geom_point(aes(color = sp), size = 2) +
  geom_smooth(aes(color = sp), se = F, method = 'lm') +
  scale_color_brewer(palette = 'Dark2') +
  #coord_cartesian(ylim = c(0,150000)) +
  theme_bw(base_size = 12) 
  



