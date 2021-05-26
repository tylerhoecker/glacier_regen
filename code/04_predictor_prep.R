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

plot(plots_pts)
as.matrix(st_distance(plots_pts)[upper.tri(st_distance(plots_pts))]) %>% 
  median()

# A wrangled dataframe of prefire and postfire tree density
model_df <- readRDS('dataframes/glac_plot_df.R') %>% 
  group_by(plot_num, season) %>% 
  #filter(sp == 'all') %>% 
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

# Reclassify a raster of just very low burn severity, based on: Harvey 2016 burn me twice
unburned <- reclassify(rdnbr, matrix(c(-250000,288,1, 288,10000,NA), ncol=3, byrow=TRUE))
# Convert to points
unburned_pts <- rasterToPoints(unburned, spatial = TRUE)

# For each plot point, find the minimum distance to any 1 of the unburned cells
model_df$dist_unburned <- st_coordinates(plots_pts) %>% 
  t() %>% 
  as.data.frame() %>% 
  as.list() %>% 
  map_df(., ~ min(pointDistance(.x, unburned_pts, lonlat = F), na.rm = T)) %>% 
  t()


  
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
                             vpd_anom = vpd - vpd_avg), .id = 'FRI') %>%  # %>% select(-precip, -tmax, -tmin, -vpd)
  mutate(plot_num = as.numeric(plot_num),
         season = factor(year + 1)) 

model_df <- model_df %>% 
  left_join(., daymet_df) 
# ------------------------------------------------------------------------------

# Soil
# ------------------------------------------------------------------------------
# Soil collected at plots and analyzed at UW Soil and Forage lab in Marshall, WI
soil_data <- read_csv(file.path(data_dir,'../Soil/glac_soil_data.csv')) %>% 
  select(plot_num, pH:TN) %>% 
  filter(!is.na(plot_num))

# Add to the modeling dataframe
model_df <- model_df %>% 
  full_join(., soil_data)

# Interpolated product from soilgrids.org
sg_clay <- list.files(paste0(gis_dir,'soilgrids/'), 
                        pattern = 'soil_clay.*', full.names = T) %>% 
  stack() %>% 
  mean() 

sg_bulk <- list.files(paste0(gis_dir,'soilgrids/'), 
                      pattern = 'soil_bulk.*', full.names = T) %>% 
  stack() %>% 
  mean() 


# Create WGS lat/long version of plot coordinates to crop down soilgrids data
points_latlong <- st_transform(plots_pts, crs = crs(sg_clay))
sg_clay <- crop(sg_clay, st_buffer(points_latlong,.01))
sg_bulk <- crop(sg_bulk, st_buffer(points_latlong,.01))

model_df$clay <- raster::extract(sg_clay, points_latlong)
model_df$bulk <- raster::extract(sg_bulk, points_latlong)
# ------------------------------------------------------------------------------

# Save this for late use
saveRDS(model_df, 'dataframes/model_df.R')


