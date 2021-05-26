library(tidyverse)
library(sf)
library(raster)

select <- dplyr::select

# Directories to data storage outside of Git repository
data_dir <- '/Users/sprague/Box/Work/PhD/Glacier/Data/'
gis_dir <- '/Users/sprague/Box/Work/PhD/GIS/glacier/'

# The complete 'raw' plot data table
plot_df <- read_csv(file.path(data_dir,'glac_regen_2019_2020.csv')) %>% 
  # A 'plot_id' made up of the site name (not all unique) and the date (combination is unique)
  mutate(plot_id = paste(name, today, sep = '-'))

# The spatial points of plot locations
plot_locs <- st_read(file.path(gis_dir,'/plots/glac_points_19_20_z12.gpkg')) %>% 
  mutate(plot_id = paste(name, today, sep = '-')) %>% 
  select(plot_id) 

# Match up sites with data with the GPS points
# Some sites that have points are missing data... will check notebook
# Some rows are duplicates because of different date formats, nbd
# Remove all of these missing/duplicate points for now
plots_w_pts <- full_join(plot_locs, plot_df) %>% 
  filter(!st_is_empty(.$geom) & plot_id != 'NA-NA') %>% 
  rownames_to_column(var = "plot_num") %>% 
  select(-name, -today) %>% 
  distinct(.keep_all = T)

# Save just the point geometries of this set for later use
plots_w_pts %>% 
  select(plot_num, plot_id, geom) %>% 
  saveRDS(., 'plot_pts.R')
  
# Just the plot-level info -----------------------------------------------------
info_df <- plots_w_pts %>% 
  rowwise() %>% 
  mutate(canopy = mean(c(canopy_1, canopy_2, canopy_3, canopy_4))) %>%
  ungroup() %>% 
  select(plot_num,plot_id,utm_easting, utm_northing, starts_with(c('plot','phys','seeds')),canopy)  
  
# Get prefire structure --------------------------------------------------------
prefire_df <- plots_w_pts %>% 
  # WRANGLE INTO DESIRED FORMAT
  select(plot_num, season, starts_with('prefire'), -contains('note')) %>% 
  pivot_longer(cols = c(starts_with('prefire'), -prefire_n_quads), 
               names_to = c('type','quad','num','att'), 
               names_sep = '_',
               values_to = 'vals', 
               values_transform = list(vals = as.character)) %>% 
  pivot_wider(names_from = 'att', values_from = 'vals') %>% 
  filter(!is.na(dbh)) %>% 
  # CONVERT CHARACTERS TO NUMBERS
  mutate(across(c(dbh,cones,serotinous), as.numeric),
  # CALCULATE BASAL AREA FROM DBH
         basal = (pi*((dbh*0.0001/2)^2)) / (0.07*(prefire_n_quads/4)), # DBH was measured in mm! m, then divide by plot area in ha
  # COMBINE PIEN ABLA
         sp = ifelse(sp %in% c('PIEN','ABLA'), 'PIAB', sp),
  # COMBINE TSHE WIT THPL ('other')
  sp = ifelse(sp %in% c('TSHE','other'), 'TSTH', sp)) %>% 
  # ADD UP COUNTS, DENS, BASAL BY PLOT AND SPECIES
  group_by(plot_num, season, prefire_n_quads, sp) %>%  #
  #summarise(number = n())
  summarise(count = n(),
            basal = sum(basal),
            dbh_mean = mean(dbh, na.rm = T),
            cones = sum(as.integer(cones), na.rm = T),
            serot = sum(serotinous, na.rm = T)) %>% 
  mutate(dens_ha = count/(0.07*(prefire_n_quads/4)) ) %>% 
  # Importance values
  group_by(plot_num) %>%
  mutate(site_basal = sum(basal),
         site_count = sum(count),
         site_density = sum(dens_ha)) %>%
  group_by(plot_num, sp) %>% 
  mutate(impval = (count/site_count) + (basal/site_basal)) %>%
  mutate(season = as.factor(season),
         time = 'prefire') %>% 
  select(time, plot_num, season, sp, basal, dbh_mean, cones, serot, dens_ha, impval)

# Save this for late use
saveRDS(prefire_df, 'prefire_df.R')

# Plot prefire species
prefire_stats <- full_join(prefire_df, select(info_df, plot_num, phys_elev)) %>% 
  mutate(plot_num = fct_reorder(plot_num, phys_elev),
         elev_class = round(phys_elev/100)) %>% 
  group_by(plot_num) %>%
  top_n(1, impval) 

sp_cols <- c('LAOC' = '#e6ab02', 'TSTH' = '#d95f02', 
             'PSME' = '#e7298a', 'PIMO' = '#66a61e', 'PIAB' = '#1b9e77',
             'PICO' = '#a6761d')

ggplot(prefire_stats) +
  geom_bar(aes(x = elev_class, fill = sp, alpha = season, color = season), width = 0.9) +
  #scale_x_discrete(labels = sort(unique(round(prefire_stats$phys_elev/100)*100))) +
  #scale_x_discrete(labels = sort(unique(prefire_df$phys.phys_elev))) +
  scale_fill_manual('Dominant species', values = sp_cols) +
  scale_alpha_manual('FRI', values = c(0.7,1)) +
  scale_color_manual('FRI', values = c('black','transparent')) +
  #scale_y_continuous(labels = c(0,1,2), breaks = c(0,1,2)) +
  theme_bw(base_size = 12) +
  labs(x = 'Plot elevation (m)', y = 'Plot count')

ggsave('prefire_sp_elev.png', height = 5, width = 7, units = 'in', dpi = 300)

# Get postfire structure -------------------------------------------------------
postfire_df <- plots_w_pts %>%
  select(plot_num, season, starts_with('seedlings'), -contains(c('header','note','end'))) %>% 
  pivot_longer(cols = c(starts_with('seedlings'), -contains('length')), 
               names_to = c('type','transect','age','sp'), 
               names_sep = '_',
               values_to = 'count') %>% 
  mutate(count = if_else(is.na(count), 0, count)) %>% 
  mutate(across(c(seedlings_t1_length, seedlings_t2_length), ~ ifelse(is.na(.), 28, .)),
         length = seedlings_t1_length + seedlings_t2_length,
         area_m2 = length * 2,
         dens_ha = (count / area_m2) * 10000) %>% 
  # COMBINE PIEN ABLA
  mutate(sp = ifelse(sp %in% c('PIEN','ABLA'), 'PIAB', sp),
         # COMBINE TSHE WIT THPL ('other')
         sp = ifelse(sp %in% c('TSHE','other'), 'TSTH', sp)) %>% 
  group_by(plot_num, season, sp) %>% 
  summarise(across(c(area_m2, count, dens_ha), sum)) %>%
  # Importance value 
  # FILL IN BASAL AREA = 0 for postfire sites
  mutate(basal = 0) %>% 
  group_by(plot_num) %>% 
  # Make site basal area = 1 for postfire sites
  mutate(site_basal = 1,
         site_count = sum(count, na.rm = T),
         site_density = sum(dens_ha, na.rm = T)) %>%
  group_by(season, plot_num, sp) %>% 
  mutate(impval = (count/site_count) + (basal/site_basal)) %>%
  mutate(impval = ifelse(is.na(impval),0,impval)) %>% 
  mutate(season = as.factor(season),
         time = 'postfire') %>% 
  select(time, plot_num, season, sp, dens_ha, basal, impval)
# %>% 
#   # REMOVE EXTREME POSTFIRE DENSITIES
#   filter(dens_ha < 500000)
# Save this for late use
saveRDS(postfire_df, 'postfire_df.R')

# Get understory cover data ----------------------------------------------------
cover_df <- plots_w_pts %>% 
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
  pivot_wider(names_from = 'type', values_from = 'cover') %>% 
  mutate(season = factor(season))

# Summarize various ways -------------------------------------------------------
# Summarize both by plot and combine ------------------------------------------
prefire_plot <- prefire_df %>% 
  group_by(plot_num, season) %>% 
  summarise(pre_dens = sum(dens_ha)) 

postfire_plot <- postfire_df %>% 
  mutate(dens_ha = ifelse(dens_ha > 500000, 500000, dens_ha)) %>% 
  group_by(plot_num, season) %>% 
  summarise(post_dens = sum(dens_ha)) 

combined_plot <- full_join(prefire_plot, postfire_plot) %>% 
  mutate(season = factor(season, levels = c('2019','2020'), 
                         labels = c('Long-interval (>200 yrs)', 'Short-interval (15 yrs)')))

combined_plot_long <- combined_plot %>% 
  pivot_longer(cols = c(pre_dens, post_dens), names_to = 'period', values_to = 'dens') 

# Summarize both by species and combine ----------------------------------------
prefire_sp <- prefire_df %>% 
  group_by(time, season, sp) %>% 
  summarise(across(c(basal, dbh_mean, dens_ha), mean)) 

postfire_sp <- postfire_df %>% 
  group_by(time, season, sp) %>% 
  summarise(across(c(dens_ha), mean)) 

combined_sp <- full_join(prefire_sp, postfire_sp)

# Fill in a dataframe that has all combinations of time and species for plotting
all_everything <- expand.grid(sp = unique(prefire_sp$sp),
                              season = as.factor(c(2019,2020)),
                              time = c('prefire','postfire'))

combined_sp <- full_join(combined_sp, all_everything) %>% 
  mutate(time = factor(time, levels = c('prefire','postfire')))

# Summarize by for late use in modeling, wide format ---------------------------
prefire_sp_wide <- prefire_df %>% 
  group_by(plot_num, season, sp) %>% 
  summarise(across(c(basal, dbh_mean, dens_ha), mean, .names = "pre_{.col}")) 

postfire_p_wide <- postfire_df %>% 
  group_by(plot_num, season, sp) %>% 
  summarise(across(c(dens_ha), mean, .names = "post_{.col}")) 

combined_sp_wide <- full_join(prefire_sp_wide, postfire_p_wide)

# Integrate with a plot-wide summary
plot_as_sp <- combined_sp_wide %>% 
  group_by(plot_num, season) %>% 
  summarise(pre_dbh_mean = mean(pre_dbh_mean, na.rm = T),
            across(c(pre_basal, pre_dens_ha, post_dens_ha), sum, na.rm = T)) %>% 
  mutate(sp = 'all')

model_df_plot_sp <- full_join(combined_sp_wide, plot_as_sp) %>% 
  # Include some plot-level metrics with this for later use
  full_join(., select(info_df, plot_num, canopy, seeds = seeds_dist_cone)) %>% 
  full_join(., cover_df)

# Save this for late use
saveRDS(model_df_plot_sp, 'glac_plot_df.R')


# Qualitative exploration of the distribution of postfire tree density
# Plotting ---------------------------------------------------------------------
ggplot(combined_sp) +
  geom_col(aes(x = sp, y = dens_ha, fill = time), position = position_dodge(width = 0.9)) +
  coord_cartesian(ylim = c(0,10000)) +
  facet_wrap(~season, labeller = labeller(season = c('2020' = 'Short', '2019' = 'Long'))) +
  scale_fill_manual(values = c('#1b9e77','#d95f02')) +
  labs(x = 'Species', y = 'Tree density') +
  theme_bw(base_size = 14)

ggplot(combined_plot_long, aes(x = season, y = dens, fill = period)) +
  geom_boxplot(varwidth = T) +
  scale_fill_manual(values = c('#d95f02','#1b9e77')) +
  coord_cartesian(ylim = c(0,30000)) +
  theme_bw(base_size = 14) +
  scale_x_discrete(breaks = c('2020','2019'), labels = c('Short', 'Long')) +
  labs(x = 'Fire return interval', y = 'Tree density')


short <- filter(combined_plot, season == '2020')
long <- filter(combined_plot, season == '2019')

cor.test(short$pre_dens,short$post_dens, method = 'spearman')
cor.test(long$pre_dens,long$post_dens, method = 'spearman')


ggplot(combined_plot, aes(x = pre_dens, y = post_dens)) +
  geom_point(aes(fill = season), shape = 21, size = 2) +
  geom_smooth(aes(color = season), method = 'lm', size = 1, se = T) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = 'lb') +
  scale_color_manual(values = c('#e7298a','#7570b3'), labels = c('Short', 'Long')) +
  scale_fill_manual(values = c('#e7298a','#7570b3'), labels = c('Short', 'Long')) +
  theme(panel.grid.minor = element_blank()) +
  theme_bw(base_size = 14) +
  labs(x = 'Prefire density (trees/ha)', y = 'Postfire density (trees/ha)')

ggsave('density_scatter_linear.png', height = 6, width = 7, units = 'in', dpi = 300)


ggplot(combined_plot) +
  geom_histogram(aes(x = pre_dens, fill = 'Prefire'), binwidth = 0.2, 
                 color = 'black') +
  geom_histogram(aes(x = post_dens, fill = 'Postfire', ), binwidth = 0.2, 
                 color = 'black', alpha = 0.8) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~season, ncol = 1) +
  scale_fill_manual(values = c('#d95f02','#1b9e77')) +
  theme_bw(base_size = 12) +
  labs(x = 'Tree density (#/ha)', y = 'Count of plots') + 
  scale_y_continuous(breaks = seq(0,10,5)) +
  theme(axis.text = element_text(color = 'black', size = 10),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('density_histograms.png', height = 8, width = 4, units = 'in', dpi = 300)


ggplot(combined_plot_long) +
  geom_histogram(aes(x = dens, fill = period), position = 'dodge', binwidth = 0.25) +
  #geom_density(aes(x = dens, fill = period), alpha = 0.7) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides = 'b') +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~season, labeller = labeller(season = c('2020' = 'Short', '2019' = 'Long'))) +
  scale_fill_manual(values = c('#d95f02','#1b9e77')) +
  theme_bw(base_size = 14) +
  labs(x = 'Tree density (#/ha)', y = 'Count of plots')

combined_plot_long %>% 
  group_by(season, period) %>% 
  summarise(across(dens, list('mean' = mean, 'med' = median, 'sd' = sd, 'n' = length)))


# Simple linear modeling: density and FRI --------------------------------------

model_df <- full_join(prefire_plot, postfire_plot) %>% 
  full_join(info_df) %>% 
  # Round densities to whole values, since they are counts per ha
  mutate(across(c(pre_dens, post_dens), round)) %>% 
  # Round extreme values
  mutate(post_dens = ifelse(post_dens > 500000, 500000, post_dens)) 

# LM
model1 <- lm(post_dens ~ pre_dens + season + season:pre_dens, data = model_df)
summary(model1)
anova(model1)

model2 <- lm(post_dens ~ pre_dens, data = model_df)
anova(model1, model2)

# GLM
testing <- glm(post_dens ~ pre_dens*season,
               family = poisson(link = "log"),
               data = model_df)
summary(testing)


## OLD -------------------------------------------------------------------------
short_df <- model_df %>% 
  filter(season == 2020)

long_df <- model_df %>% 
  filter(season == 2019)

short_model <- lm(log10(post_dens + 1) ~ log10(pre_dens + 1),
                  data = short_df) 
summary(short_model)

long_model <- lm(log10(post_dens + 1) ~ log10(pre_dens + 1),
                 data = long_df) 
summary(long_model)


%>% 
  mutate(dev_ne = cos((45*pi/180) - (phys_aspect*pi/180) ) + 1) %>% 
  rowwise() %>% 
  mutate(min_seed = min(c(seeds_north,seeds_east,seeds_south,seeds_west))) %>% 
  ungroup() %>% 
  mutate(seeds_cone_serotinous = ifelse(is.na(seeds_cone_serotinous), 0, seeds_cone_serotinous))
testing <- glm(post_dens + 1 ~ season + pre_dens + 
                 dev_ne + phys_elev + 
                 seeds_dist_cone + seeds_cone_serotinous +
                 canopy,
               data = model_df, quasipoisson(link = "log"))



  pivot_longer(c(starts_with('groundcover','vegcover')))
  
  
  
  dplyr::select(today, starts_with('plot'), starts_with('seedlings'), phys.phys_elev, 
                -contains('header'), -contains('other'), -contains('end')) %>% 
  gather(key, value, -today, -contains('length'), -starts_with('plot'), -starts_with('phys')) %>% 
  separate(key, into = c('del1','del2','del3','del4','transect','year','sp'), sep = "_") %>%
  select(c('today', 'del1','del2','del3','del4','transect','year','sp'))
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
  



