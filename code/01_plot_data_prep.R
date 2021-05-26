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
  select(plot_num, plot_id, geom) 

saveRDS(plots_w_pts, 'dataframes/plot_pts.R')

zone12_coords <- data.frame('plot_num' = plots_w_pts$plot_num,
                            'utm_x' = st_coordinates(plots_w_pts)[,1],
                            'utm_y' = st_coordinates(plots_w_pts)[,2])

# Just the plot-level info -----------------------------------------------------
info_df <- plots_w_pts %>% 
  rowwise() %>% 
  mutate(canopy = mean(c(canopy_1, canopy_2, canopy_3, canopy_4))) %>%
  ungroup() %>% 
  select(plot_num,plot_id, starts_with(c('plot','phys','seeds')),canopy) %>% 
  full_join(., zone12_coords)

saveRDS(info_df, 'dataframes/info_df.R')

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
         # DBH was measured in mm! convert to m, then divide by plot area in ha to get m2/ha
         basal = (pi*((dbh*0.001/2)^2)) / (0.07*(prefire_n_quads/4)),
         sp = ifelse(sp %in% c('other'), 'THPL', sp)) %>% 
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
saveRDS(prefire_df, 'dataframes/prefire_df.R')


# Get postfire structure -------------------------------------------------------
postfire_df <- plots_w_pts %>%
  select(plot_num, season, starts_with('seedlings'), -contains(c('header','note','end'))) %>% 
  pivot_longer(cols = c(starts_with('seedlings'), -contains('length')), 
               names_to = c('type','transect','age','sp'), 
               names_sep = '_',
               values_to = 'count') %>% 
  # a new column for 'other' species was added manually to the raw data file
  filter(sp != 'other') %>% 
  mutate(count = if_else(is.na(count), 0, count)) %>% 
  mutate(across(c(seedlings_t1_length, seedlings_t2_length), ~ ifelse(is.na(.), 28, .)),
         length = seedlings_t1_length + seedlings_t2_length,
         area_m2 = length * 2,
         dens_ha = (count / area_m2) * 10000) %>% 
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
saveRDS(postfire_df, 'dataframes/postfire_df.R')

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

# Summarize by for late use in modeling, wide format ---------------------------
prefire_sp_wide <- prefire_df %>% 
  group_by(plot_num, season, sp) %>% 
  summarise(across(c(basal, dens_ha, serot), mean, .names = "pre_{.col}")) 

postfire_sp_wide <- postfire_df %>% 
  group_by(plot_num, season, sp) %>% 
  summarise(across(c(dens_ha), mean, .names = "post_{.col}")) 

combined_sp_wide <- full_join(prefire_sp_wide, postfire_sp_wide) %>% 
  mutate(across(c(pre_basal, pre_dens_ha, pre_serot, post_dens_ha), ~ ifelse(is.na(.x),0,.x))) %>% 
  pivot_wider(names_from = sp, values_from = c(pre_basal, pre_dens_ha, pre_serot, post_dens_ha),
              values_fill = 0) %>% 
  # Make serotiny binary and exclude species that aren't serotinous
  mutate(serot = ifelse(pre_serot_PICO > 0, 1, 0)) %>% 
  select(-contains('pre_serot')) 

# Integrate with a plot-wide summary
plot_all_sp <- full_join(prefire_sp_wide, postfire_sp_wide) %>% 
  group_by(plot_num, season) %>% 
  summarise(across(c(pre_basal, pre_dens_ha, post_dens_ha), sum, na.rm = T)) %>% 
  mutate(sp = 'all') %>% 
  pivot_wider(names_from = sp, values_from = c(pre_basal, pre_dens_ha, post_dens_ha)) 

plot_df <- full_join(combined_sp_wide, plot_all_sp) %>% 
  # Include some plot-level metrics with this for later use
  full_join(., select(info_df, plot_num, canopy, seeds = seeds_dist_cone)) %>% 
  full_join(., cover_df)

# Save this for late use
saveRDS(plot_df, 'dataframes/glac_plot_df.R')


# Check for spatial autocorrelation before proceeding ------------------------
# Result is that no evidence for spatial AR in postfire total density
library(gstat)
plot_df <- readRDS('dataframes/glac_plot_df.R') %>% 
  full_join(info_df)
coordinates(plot_df) <- ~ utm_x+utm_y
crs(plot_df) <- "+init=epsg:32612"
plot(plot_df)

gstat_df <- gstat(formula = post_dens_ha_all~1, 
                  locations = plot_df, data = plot_df)

# Make variogram
max_dist <- (max(plot_df$utm_y) - min(plot_df$utm_y)) / 2
vg <- variogram(gstat_df, cutoff = max_dist, width = max_dist/50) 

ggplot() +
  geom_point(data = vg, aes(x = dist, y = gamma)) +
  #geom_line(data = vg_line, aes(x = dist, y = gamma)) +
  labs(y = "Semivariance (gamma)", x = "Lag distance (m)") +
  ggtitle('Spatial autocorrelation in postfire density') +
  theme_bw() 



