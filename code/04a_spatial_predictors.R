# SPATIALLY CONTINUOUS PREDICTORS
# Prediction grid - use dem cropped to the fires as a master grid for spatially continuous predictions
# ------------------------------------------------------------------------------
# Load 100m DEM for Glacier
dem <- raster(file.path(gis_dir,'dems/dem_100m_glac_20km.tif'))
rdnbr <- raster(file.path(gis_dir,'../fire_data/howeridge_sprague_rdnbr.tif'))

# dem and rdnbr were loaded above
pred_grid <- crop(dem, rdnbr)
pred_pts <- rasterToPoints(pred_grid)


# Which variables are important? Based on models run in script 06:
# 

# Topographic
topo_vars <- stack()
