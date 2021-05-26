library(tidyverse)
select <- dplyr::select


model_df <- readRDS('dataframes/model_df.R')

# Plot distributions - contrasting short and long
# ------------------------------------------------------------------------------
# Biotic predictors
biotic_labs <- c('pre_basal' = 'Prefire basal area (m2)', 
                 'canopy' = 'Residual canopy (%)',
                 'seeds' = 'Dist. seed source (m)'
                 #'forb' = 'Forb cover (%)',
                 #'gram' = 'Graminoid cover (%)',
                 #'shrub' = 'Shrub cover (%)'
)

biotic_df <- model_df %>% 
  ungroup() %>% 
  select(season, pre_basal, canopy, seeds) %>% #forb, gram, shrub
  pivot_longer(-season, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = factor(variable, 
                           levels = names(biotic_labs), 
                           labels = biotic_labs))
ggplot(biotic_df) +
  geom_histogram(data = filter(biotic_df, season == 2019), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10) +
  geom_histogram(data = filter(biotic_df, season == 2020), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10, alpha = 0.8) +
  facet_grid(~variable, scales = 'free', switch = 'x') +
  theme_bw(base_size = 12) +
  scale_color_manual('FRI', values = c('#524f80','#961b5b'), labels = c('Long', 'Short')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a'), labels = c('Long', 'Short')) + #, labels = c('Long', 'Short')
  labs(y = 'Number of plots') +
  theme(axis.title.x = element_blank(),
        strip.background =  element_blank(),
        strip.text = element_text(size = 12),
        strip.placement.x = 'outside')

ggsave('biotic_histograms.png', height = 4, width = 7, units = 'in', dpi = 300)

# ------------------------------------------------------------------------------
# Topographic predictors
topo_labs <- c('dev_ne' = 'Aspect', 
               'tpi' = 'Topo. position',
               'tri' = 'Roughness')  

topo_df <- model_df %>% 
  ungroup() %>% 
  select(season, dev_ne, tpi, tri) %>% #forb, gram, shrub
  pivot_longer(-season, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = factor(variable, 
                           levels = names(topo_labs), 
                           labels = topo_labs))
ggplot(topo_df) +
  geom_histogram(data = filter(topo_df, season == 2019), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10) +
  geom_histogram(data = filter(topo_df, season == 2020), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10,alpha = 0.8) +
  facet_grid(~variable, scales = 'free', switch = 'x') +
  theme_bw() +
  scale_color_manual('FRI', values = c('#524f80','#961b5b'), labels = c('Long', 'Short')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a'), labels = c('Long', 'Short')) + #, labels = c('Long', 'Short')
  labs(y = 'Number of plots') +
  theme(axis.title.x = element_blank(),
        strip.background =  element_blank(),
        strip.text = element_text(size = 8),
        strip.placement.x = 'outside')  

ggsave('topo_histograms.png', height = 4, width = 7, units = 'in', dpi = 300)

# Soil predictors
soil_labs <- c('rdnbr' = 'Burn severity',
               'OM' = 'Soil OM',
               'TN' = 'Soil total N')  

soil_df <- model_df %>% 
  ungroup() %>% 
  select(season, rdnbr, OM, TN) %>% #forb, gram, shrub
  pivot_longer(-season, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = factor(variable, 
                           levels = names(soil_labs), 
                           labels = soil_labs))
ggplot(soil_df) +
  geom_histogram(data = filter(soil_df, season == 2019), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10) +
  geom_histogram(data = filter(soil_df, season == 2020), 
                 aes(x = value, fill = season, color = season), 
                 bins = 10,alpha = 0.8) +
  facet_grid(~variable, scales = 'free', switch = 'x') +
  theme_bw() +
  scale_color_manual('FRI', values = c('#524f80','#961b5b'), labels = c('Long', 'Short')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a'), labels = c('Long', 'Short')) + #, labels = c('Long', 'Short')
  labs(y = 'Number of plots') +
  theme(axis.title.x = element_blank(),
        strip.background =  element_blank(),
        strip.text = element_text(size = 8),
        strip.placement.x = 'outside')  

ggsave('soil_histograms.png', height = 4, width = 7, units = 'in', dpi = 300)


# ------------------------------------------------------------------------------ 

# Climate predictors
clim_labs <- c('precip' = 'Precip. (mm)', 
               #'precip_anom' = 'Precip. anom. (mm)', 
               'tmax' = 'Max. temp. (deg. C)',
               #'tmax_anom' = 'Max. temp. anom. (deg. C)',
               'tmin' = 'Min. temp. (deg. C)',
               #'tmin_anom' = 'Min. temp. anom (deg. C)',
               'vpd' = 'VPD'#,
               #'vpd_anom' = 'VPD anom.'
               ) 



clim_df <- model_df %>% 
  ungroup() %>% 
  select(season, precip_avg, precip_anom, tmax_avg, tmax_anom, tmin_avg, tmin_anom, vpd_avg, vpd_anom) %>% #forb, gram, shrub
  pivot_longer(-season, names_to = c('variable','metric'), names_sep = '_', values_to = 'value') %>% 
  #pivot_longer(-season, names_to = 'variable', values_to = 'value') %>% 
  mutate(variable = factor(variable, 
                           levels = names(clim_labs), 
                           labels = clim_labs))
ggplot(clim_df) +
  geom_histogram(data = filter(clim_df, season == 2019), 
                 aes(x = value, fill = season, color = season), 
                 bins = 20) +
  geom_histogram(data = filter(clim_df, season == 2020), 
                 aes(x = value, fill = season, color = season), 
                 bins = 20, alpha = 0.8) +
  facet_wrap(metric~variable, scales = 'free_x', ncol = 4) +
  theme_bw() +
  scale_color_manual('FRI', values = c('#524f80','#961b5b'), labels = c('Long', 'Short')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a'), labels = c('Long', 'Short')) + #, labels = c('Long', 'Short')
  labs(y = 'Number of plots') +
  theme(axis.title.x = element_blank(),
        strip.background =  element_blank(),
        strip.text = element_text(size = 8),
        strip.placement.x = 'outside')  

ggsave('climate_histograms.png', height = 6, width = 9, units = 'in', dpi = 300)

# MODELING
# ------------------------------------------------------------------------------

# Setting up a dataframe to feed the models, excluding some variables right off 
# the bat, truncating extreme values and rescaling to zero centered.
rescaled_df <- model_df %>% 
  ungroup() %>% 
  # Remove the missing soil data sites
  filter(!is.na(pH)) %>% 
  # mutate(tavg = (tmax + tmin) /2,
  #        dev_ne = cos((45*pi/180) - (aspect*pi/180) ) + 1) %>% 
  select(-plot_num, -sp, -season, -plot_id, -FRI, -year, -Sikora, -pH, -P, -K, -total, -tree) %>% #-tmin, -tmax , -Sikora, -year, -elev, -tri, -slope, -TN, -pH, -P, -K, -tavg
  # Truncate the very large post-fire denisty values from one site
  # 250,000 stems/ha is >97th percentile
  mutate(post_dens_ha = ifelse(post_dens_ha > 250000, 250000, post_dens_ha)) %>% 
  mutate(across(c(everything() & !post_dens_ha), scale))  

# Create subset matrix that will exclude colinear terms
### r > 0.5
cormat <- abs(cor(select(rescaled_df, -post_dens_ha), method = 'spearman')) <= 0.5
cormat[!lower.tri(cormat)] <- NA

# Visualize pairwise... this is slow and not useful with so many terms... but keeping in case
# model_df %>% 
#   ungroup() %>% 
#   filter(!is.na(pH)) %>% 
#   select(-plot_num, -sp, -season, -plot_id, -FRI, -year) %>%
#   GGally::ggpairs()

# A all-subset model selection procedure predicting total postfire density
library(MuMIn)
# ------------------------------------------------------------------------------
