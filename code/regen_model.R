# Load packages
libs <- list('tidyverse','raster','daymetr','sf')
lapply(libs, library, character.only = T)
select <- dplyr::select

# Directories to data storage outside of Git repository
data_dir <- '/Users/sprague/Box/Work/PhD/Glacier/Data/'
gis_dir <- '/Users/sprague/Box/Work/PhD/GIS/glacier/'

# Load the plot locations and data... must have run 'regen_plot_explore.R' first
# ------------------------------------------------------------------------------
# Just the point geometries of this set
plots_pts <- readRDS('dataframes/plot_pts.R')

# A wrangled dataframe of prefire and postfire tree density
model_df <- readRDS('dataframes/glac_plot_df.R') %>% 
  group_by(plot_num, season) %>% 
  filter(sp == 'all') %>% 
  # pivot_wider(names_from = sp, values_from = c(pre_basal,pre_dbh_mean, pre_dens_ha, post_dens_ha)) %>% 
  # This is key to making sure the points are in the same order!!
  mutate(plot_num = as.numeric(plot_num)) %>% 
  arrange(plot_num)
# ------------------------------------------------------------------------------

# Topographic data
# ------------------------------------------------------------------------------
# Load 100m DEM for Glacier
dem <- raster(file.path(gis_dir,'dems/dem_100m_glac_20km.tif'))
# Calculate various terrain indices
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
slope <- terrain(dem, opt = 'slope', unit = 'degrees')
tpi <- terrain(dem, opt = 'tpi', unit = 'degrees')
tri <- terrain(dem, opt = 'tri', unit = 'degrees')

# Add them to the dataframe of predictors and responses
model_df$elev <- raster::extract(dem, plots_pts)
# Convert aspect to continuous measure per Beers et al. 1966 J. For.
model_df$dev_ne <- cos( (45*pi/180) - (raster::extract(aspect, plots_pts)*pi/180) ) + 1 
#model_df$aspect <- raster::extract(aspect, plots_pts) 
model_df$slope <- raster::extract(slope, plots_pts)
model_df$tpi <- raster::extract(tpi, plots_pts)
model_df$tri <- raster::extract(tri, plots_pts)
# ------------------------------------------------------------------------------

# Burn severity data 
# These data were prepped from data downloaded for each file from https://www.mtbs.gov/direct-download > Fire Bundles
# Then in 'clip_mtbs.R' they were clipped to the extent of the study area and merged into 1 file
# They are rdNBR values
# ------------------------------------------------------------------------------
rdnbr <- raster(file.path(gis_dir,'../fire_data/howeridge_sprague_rdnbr.tif'))
model_df$rdnbr <- raster::extract(rdnbr, plots_pts)
# ------------------------------------------------------------------------------

# Climate data
# ------------------------------------------------------------------------------
#plot_nums <- plots_pts$plot_num

# Make a table in this format for downloading
# plots_pts %>% 
#   st_transform(x, crs = 4326) %>% 
#   st_coordinates() %>% 
#   as.data.frame() %>% 
#   mutate(site = plot_nums) %>% 
#   select(site,lat=Y,lon=X) %>% 
#   #view()
#   write_csv(., 'plots_for_daymet.csv')
#  
# The downloading part
# daymet_dl <-
#   download_daymet_batch(
#     file_location = 'plots_for_daymet.csv',
#     start = 1989,
#     end = 2020,
#     internal = TRUE)
# saveRDS(daymet_dl, 'daymet_dl.R')
# Load from previously downloaded to save time!
daymet_dl <- readRDS('dataframes/daymet_dl.R')

# Vapor pressre deficit equations
# to calc avg daily vpd, following FAO guidelines for meteorological data
# www.fao.org/docrep/X0490E/x0490e07.htm

calc_es = function(temp) {
  es = ifelse(temp < 0,
              0.61078 * exp((21.875 * temp) / (temp + 265.5)),  
              0.61078 * exp((17.269 * temp) / (temp + 237.3)))  
  return(es)  
}

calc_vpd = function(min_temp, max_temp, vp) {  # temp in C
  # first calc sat vapor pressure at min and max temp
  es_min = calc_es(min_temp)
  es_max = calc_es(max_temp)
  
  # average value
  es = (es_min + es_max) / 2
  
  # vpd is difference between saturated vp (es) and actual vp (provided by Daymet)
  vpd = es - vp
  
  return(vpd)
}


# Calculate annual average JJA values for a few variables
daymet <- map_df(daymet_dl, 
              function(x){
                x$data %>% 
                  rename(precip = `prcp..mm.day.`, 
                         tmax = `tmax..deg.c.`,
                         tmin = `tmin..deg.c.`,
                         vp = `vp..Pa.`) %>%
                  # day 150 = may 30, day 244 = sept 1 in most(?) years
                  filter(yday > 150 & yday < 244) %>% 
                  # VPD for each day - first convert vp to kPa!
                  mutate(vp_kPa = vp/1000,
                         vpd = calc_vpd(tmin, tmax, vp_kPa)) %>% 
                  group_by(year) %>% 
                  summarise(across(c(precip, tmax, tmin, vpd), mean)) %>% 
                  mutate(plot_num = x$site)
                }) 

# Separate this into a list of two dataframes with only the appropriate years for each fire
daymet_fires <- list('short' = filter(daymet, year > 1989), 
                     'long' = filter(daymet, year < 2019))

# Map the same sequence to calculate the average for these 30 years and anomaly of first year postfire 
daymet_df <- map_df(daymet_fires, ~.x %>% 
                      group_by(plot_num) %>% 
                      mutate(across(c(precip, tmax, tmin, vpd), mean, .names = "{.col}_avg")) %>% 
                      filter(year == max(.x$year)) %>% 
                      mutate(precip_anom = precip - precip_avg,
                             tmax_anom = tmax - tmax_avg,
                             tmin_anom = tmin - tmin_avg,
                             vpd_anom = vpd - vpd_avg) %>% 
                      select(-precip, -tmax, -tmin, -vpd), .id = 'FRI') %>% 
  mutate(plot_num = as.numeric(plot_num))

model_df <- model_df %>% 
  left_join(., daymet_df)
# ------------------------------------------------------------------------------

# Soil
# ------------------------------------------------------------------------------
soil_data <- read_csv(file.path(data_dir,'../Soil/glac_soil_data.csv')) %>% 
  select(plot_num, pH:TN) %>% 
  filter(!is.na(plot_num))

model_df <- model_df %>% 
  full_join(., soil_data)
# ------------------------------------------------------------------------------

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

# Topoedaphic predictors
topo_labs <- c('dev_ne' = 'Aspect', 
                  'tpi' = 'Topo. position',
                  'tri' = 'Roughness',
                  'rdnbr' = 'Burn severity',
                  'OM' = 'Soil OM',
                  'TN' = 'Soil total N')  
  
topo_df <- model_df %>% 
  ungroup() %>% 
  select(season, dev_ne, tpi, tri, rdnbr, OM, TN) %>% #forb, gram, shrub
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
# ------------------------------------------------------------------------------ 
  
# Climate predictors
clim_labs <- c('precip_avg' = 'Precip. avg. (mm)', 
               'precip_anom' = 'Precip. anom. (mm)', 
               'tmax_avg' = 'Max. temp. avg. (deg. C)',
               'tmax_anom' = 'Max. temp. anom. (deg. C)',
               'tmin_avg' = 'Min. temp. avg. (deg. C)',
               'tmin_anom' = 'Min. temp. anom (deg. C)',
               'vpd_avg' = 'VPD avg.',
               'vpd_anom' = 'VPD anom.')  

clim_df <- model_df %>% 
  ungroup() %>% 
  select(season, precip_avg, precip_anom, tmax_avg, tmax_anom, tmin_avg, tmin_anom, vpd_avg, vpd_anom) %>% #forb, gram, shrub
  pivot_longer(-season, names_to = 'variable', values_to = 'value') %>% 
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
  facet_grid(~variable, scales = 'free', switch = 'x') +
  theme_bw() +
  scale_color_manual('FRI', values = c('#524f80','#961b5b'), labels = c('Long', 'Short')) +
  scale_fill_manual('FRI', values = c('#7570b3','#e7298a'), labels = c('Long', 'Short')) + #, labels = c('Long', 'Short')
  labs(y = 'Number of plots') +
  theme(axis.title.x = element_blank(),
        strip.background =  element_blank(),
        strip.text = element_text(size = 8),
        strip.placement.x = 'outside')  


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
# Biotic model selection -------------------------------------------------------
glob_biotic <- glm(round(post_dens_ha) ~ pre_basal + pre_dbh_mean + pre_dens_ha + seeds + forb + gram + shrub,
                   family = MASS::negative.binomial(link = "log", theta = 1),
                   data = rescaled_df,
                   na.action = 'na.fail')


# Now perform dredge. Max # of predictors = 7. Rank by AICc. Exlcude based on cormat.
# This function is used in the calculation of QAIC: https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
dfun <- function(object){with(object,sum((weights * residuals^2)[weights > 0])/df.residual)}

# Only noticeable difference with QAIC is that is considers two models equally supported instead of 1.
dredge_biotic <- dredge(
  glob_biotic, 
  m.lim = c(1,3),
  subset = cormat,
  rank = 'QAIC', chat = dfun(glob_mod)
  )

avg_biotic <- model.avg(dredge_biotic, subset = delta <= 2)
confint(avg_biotic)

# Abiotic model selection ------------------------------------------------------
glob_abiotic <- glm(round(post_dens_ha) ~ canopy + elev + dev_ne + slope + tpi + tri + rdnbr + precip_avg + tmax_avg + tmin_avg + vpd_avg + precip_anom + tmax_anom + tmin_anom + vpd_anom + OM + TN,
                   family = MASS::negative.binomial(link = "log", theta = 1),
                   data = rescaled_df,
                   na.action = 'na.fail')


# Now perform dredge. Max # of predictors = 7. Rank by AICc. Exlcude based on cormat.
# This function is used in the calculation of QAIC: https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
dfun <- function(object){with(object,sum((weights * residuals^2)[weights > 0])/df.residual)}

# Only noticeable difference with QAIC is that is considers two models equally supported instead of 1.
dredge_abiotic <- dredge(
  glob_abiotic, 
  m.lim = c(1,5),
  subset = cormat,
  rank = 'QAIC', chat = dfun(glob_mod)
)

avg_biotic <- model.avg(dredge_biotic, subset = delta <= 2)

# Combine
model_results <- coefTable(avg_obj) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'predictor') %>% 
  mutate(lower = confint(avg_obj)[,1],
         upper = confint(avg_obj)[,2]) %>% 
  select(predictor, estimate = Estimate, lower, upper) %>% 
  filter(predictor != '(Intercept)') %>% 
  mutate(predictor = factor(predictor),
         predictor = fct_reorder(predictor, estimate, .desc = T),
         type = case_when(predictor %in% c('canopy','seeds','pre_basal') ~ 'Prefire structure', #'pre_dens_ha','pre_dbh_mean'
                          predictor %in% c('tpi','aspect') ~ 'Topoedaphic', #'OM',
                          predictor %in% c('tmax','vpd') ~ 'Climate'), #'tmin',,'precip' predictor %in% c('rdnbr') ~ 'Fire')
         predictor = fct_recode(predictor,
                                'Dist. to seed' = 'seeds',
                                'Canopy cover' = 'canopy',
                                #'Min. temp. (JJA)' = 'tmin',
                                'Topo. position' = 'tpi',
                                #'Prefire density' = 'pre_dens_ha',
                                'Max. temp. (JJA)' = 'tmax',
                                'Aspect (NE-ness)' = 'aspect',
                                'Prefire basal area' = 'pre_basal',
                                #'Prefire tree diam.' = 'pre_dbh_mean',
                                #'Soil OM' = 'OM',
                                #'Precip. (JJA)' = 'precip',
                                #'Burn severity' = 'rdnbr',
                                'VPD (JJA)' = 'vpd'))
  
# ------------------------------------------------------------------------------

# Plotting
# ------------------------------------------------------------------------------
type_colors = c('#377eb8','#4daf4a','#984ea3') #fire/red: '#e41a1c',
 
ggplot(model_results, aes(x = predictor, y = estimate)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = type), width = 0, 
                position = position_dodge(width=0.7), size = 0.7) +
  geom_point(aes(fill = type), position = position_dodge(width=0.7), 
             stroke = 1.5, size = 3, shape = 21) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  coord_flip(ylim = c(-3, 3)) + 
  theme_bw(base_size = 16) +
  labs(y = 'Effect size (log-odds)', x = '') +
  scale_color_manual('Type', values = type_colors) +
  scale_fill_manual('Type', values = type_colors) +
  theme(title = element_text(size = 12)) +
  theme(axis.text = element_text(color = 'black'),
        strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))

ggsave('glm_effects.png', height = 5, width = 7, units = 'in', dpi = 300)










# OLD
# Terms that will be allowed to interact with season
intrxn_vars <- paste(names(select(model_df, -post_dens_ha, -season, -pre_dbh_mean, -pre_basal, -pre_dens_ha)), 
                     collapse = '+')
# Terms that won't interact with FRI... because these reflect FRI already
other_vars <- paste(names(select(model_df, pre_dbh_mean, pre_basal, pre_dens_ha)), 
                    collapse = '+')

glob_form <- as.formula(paste0('round(post_dens_ha) ~ 
                               season*(',intrxn_vars,') + ',
                               other_vars))

# Try without interactions first 
glob_form <- as.formula(paste0('round(post_dens_ha) ~ ',
                               paste(names(select(rescaled_df, -post_dens_ha)), collapse = '+')))

# glob_mod <- glm(glob_form, 
#                 family = MASS::negative.binomial(link = "log", theta = 2),
#                 data = model_df,
#                 na.action = 'na.fail')




