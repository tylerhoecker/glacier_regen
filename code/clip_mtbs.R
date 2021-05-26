# This is not a stand-alone script. It uses things that were created in QGIS 
# to clip MTBS rasters to the study area and merge them

rast_crs <- crs(raster('/Users/sprague/Box/Work/PhD/GIS/fire_data/mtbs_MT_2018.tif'))

utm_crs <- crs(st_read('/Users/sprague/Box/Work/PhD/GIS/glacier/fire_perims/HoweRidgeReburn.gpkg'))


perimeters <- map(list('/Users/sprague/Box/Work/PhD/GIS/glacier/fire_perims/HoweRidgeReburn.gpkg',
                      '/Users/sprague/Box/Work/PhD/GIS/glacier/fire_perims/Sprague2017Fire.gpkg'),
                 ~ st_read(.x) %>% 
                   st_geometry() %>% 
                   as('Spatial') %>% 
                   spTransform(.,CRSobj = rast_crs))
  

mtbs_layers <- map2(list('/Users/sprague/Box/Work/PhD/GIS/fire_data/howeridge/howeridge_rdnbr.tif',
                          '/Users/sprague/Box/Work/PhD/GIS/fire_data/sprague/sprague_rdnbr.tif'),
                    perimeters,
                    ~ raster(.x) %>% 
                        crop(., .y) %>% 
                        mask(., .y) 
                    )

mtbs_merged <- mosaic(mtbs_layers[[1]],mtbs_layers[[2]], fun = max) 


mtbs_utm <- projectRaster(mtbs_merged, crs = utm_crs)

library(tmap)
tm_shape(mtbs_utm) +
  tm_raster(breaks = seq(-500,1500,100), midpoint = NA) +
  tm_layout(legend.outside = T) +
  tm_shape(plot_locs) +
  tm_bubbles(size = .1)

writeRaster(mtbs_utm, filename = '/Users/sprague/Box/Work/PhD/GIS/fire_data/howeridge_sprague_rdnbr.tif')


