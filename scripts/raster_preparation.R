.libPaths("/home/davidpw/R/lib/3.6")

library(raster)

##########################
### Load Current Stack ###
##########################

### Only layers that passed the corr/VIF tests

current_stack <- stack("env_data/current/Present.Benthic.Min.Depth.Salinity.Mean.tif",
                       "env_data/current/Present.Benthic.Min.Depth.Temperature.Lt.max.tif",
                       "env_data/current/Present.Benthic.Min.Depth.Temperature.Lt.min.tif",
                       "env_data/current/Present.Surface.Ice.thickness.Lt.max.tif",
                       "env_data/current/Present.Surface.Ice.thickness.Lt.min.tif")

##########################
### Load Future Stacks ###
##########################

RCP26_2050_stack <- stack("env_data/RCP26_2050/2050AOGCM.RCP26.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP26_2050/2050AOGCM.RCP26.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP26_2050/2050AOGCM.RCP26.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP26_2050/2050AOGCM.RCP26.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP26_2050/2050AOGCM.RCP26.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP26_2100_stack <- stack("env_data/RCP26_2100/2100AOGCM.RCP26.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP26_2100/2100AOGCM.RCP26.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP26_2100/2100AOGCM.RCP26.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP26_2100/2100AOGCM.RCP26.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP26_2100/2100AOGCM.RCP26.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP45_2050_stack <- stack("env_data/RCP45_2050/2050AOGCM.RCP45.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP45_2050/2050AOGCM.RCP45.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP45_2050/2050AOGCM.RCP45.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP45_2050/2050AOGCM.RCP45.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP45_2050/2050AOGCM.RCP45.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP45_2100_stack <- stack("env_data/RCP45_2100/2100AOGCM.RCP45.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP45_2100/2100AOGCM.RCP45.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP45_2100/2100AOGCM.RCP45.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP45_2100/2100AOGCM.RCP45.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP45_2100/2100AOGCM.RCP45.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP60_2050_stack <- stack("env_data/RCP60_2050/2050AOGCM.RCP60.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP60_2050/2050AOGCM.RCP60.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP60_2050/2050AOGCM.RCP60.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP60_2050/2050AOGCM.RCP60.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP60_2050/2050AOGCM.RCP60.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP60_2100_stack <- stack("env_data/RCP60_2100/2100AOGCM.RCP60.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP60_2100/2100AOGCM.RCP60.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP60_2100/2100AOGCM.RCP60.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP60_2100/2100AOGCM.RCP60.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP60_2100/2100AOGCM.RCP60.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP85_2050_stack <- stack("env_data/RCP85_2050/2050AOGCM.RCP85.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP85_2050/2050AOGCM.RCP85.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP85_2050/2050AOGCM.RCP85.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP85_2050/2050AOGCM.RCP85.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP85_2050/2050AOGCM.RCP85.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

RCP85_2100_stack <- stack("env_data/RCP85_2100/2100AOGCM.RCP85.Benthic.Min.Depth.Salinity.Mean.tif.BOv2_1.tif",
                          "env_data/RCP85_2100/2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Lt.max.tif.BOv2_1.tif",
                          "env_data/RCP85_2100/2100AOGCM.RCP85.Benthic.Min.Depth.Temperature.Lt.min.tif.BOv2_1.tif",
                          "env_data/RCP85_2100/2100AOGCM.RCP85.Surface.Ice.thickness.Lt.max.tif.BOv2_1.tif",                
                          "env_data/RCP85_2100/2100AOGCM.RCP85.Surface.Ice.thickness.Lt.min.tif.BOv2_1.tif")

names(RCP26_2050_stack) <-
  names(RCP26_2100_stack) <-
  names(RCP45_2050_stack) <- 
  names(RCP45_2100_stack) <-
  names(RCP60_2050_stack) <-
  names(RCP60_2100_stack) <-
  names(RCP85_2050_stack) <- 
  names(RCP85_2100_stack) <- 
  names(current_stack)

#################
### Load Mask ###
#################

mask <- raster("env_data/mask/coastline_bathymetry_shoreline_mask.tif")

###################
### Crop Stacks ###
###################

current_stack <- crop(current_stack,
                      extent(c(-180,180,30,90)))

RCP26_2050_stack <- crop(RCP26_2050_stack,
                         extent(c(-180,180,30,90)))

RCP26_2100_stack <- crop(RCP26_2100_stack,
                         extent(c(-180,180,30,90)))

RCP45_2050_stack <- crop(RCP45_2050_stack,
                         extent(c(-180,180,30,90)))

RCP45_2100_stack <- crop(RCP45_2100_stack,
                         extent(c(-180,180,30,90)))

RCP60_2050_stack <- crop(RCP60_2050_stack,
                         extent(c(-180,180,30,90)))

RCP60_2100_stack <- crop(RCP60_2100_stack,
                        extent(c(-180,180,30,90)))

RCP85_2050_stack <- crop(RCP85_2050_stack,
                         extent(c(-180,180,30,90)))

RCP85_2100_stack <- crop(RCP85_2100_stack,
                         extent(c(-180,180,30,90)))

########################
### Reproject Stacks ###
########################

current_stack <- projectRaster(current_stack,
                               crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP26_2050_stack <- projectRaster(RCP26_2050_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP26_2100_stack <- projectRaster(RCP26_2100_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP45_2050_stack <- projectRaster(RCP45_2050_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP45_2100_stack <- projectRaster(RCP45_2100_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP60_2050_stack <- projectRaster(RCP60_2050_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP60_2100_stack <- projectRaster(RCP60_2100_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP85_2050_stack <- projectRaster(RCP85_2050_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

RCP85_2100_stack <- projectRaster(RCP85_2100_stack,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

##########################
### Mask Raster Stacks ###
##########################

current_stack <- mask(current_stack,
                      mask)

RCP26_2050_stack <- mask(RCP26_2050_stack,
                         mask)

RCP26_2100_stack <- mask(RCP26_2100_stack,
                         mask)

RCP45_2050_stack <- mask(RCP45_2050_stack,
                         mask)

RCP45_2100_stack <- mask(RCP45_2100_stack,
                         mask)

RCP60_2050_stack <- mask(RCP60_2050_stack,
                         mask)

RCP60_2100_stack <- mask(RCP60_2100_stack,
                         mask)

RCP85_2050_stack <- mask(RCP85_2050_stack,
                         mask)

RCP85_2100_stack <- mask(RCP85_2100_stack,
                         mask)

####################
### Save to File ###
####################

writeRaster(current_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/current/%s.tif",
                               names(current_stack)),
            overwrite = TRUE)

writeRaster(RCP26_2050_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP26_2050/%s.tif",
                               names(RCP26_2050_stack)),
            overwrite = TRUE)

writeRaster(RCP26_2100_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP26_2100/%s.tif",
                               names(RCP26_2100_stack)),
            overwrite = TRUE)

writeRaster(RCP45_2050_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP45_2050/%s.tif",
                               names(RCP45_2050_stack)),
            overwrite = TRUE)

writeRaster(RCP45_2100_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP45_2100/%s.tif",
                               names(RCP45_2100_stack)),
            overwrite = TRUE)

writeRaster(RCP60_2050_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP60_2050/%s.tif",
                               names(RCP60_2050_stack)),
            overwrite = TRUE)

writeRaster(RCP60_2100_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP60_2100/%s.tif",
                               names(RCP60_2100_stack)),
            overwrite = TRUE)

writeRaster(RCP85_2050_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP85_2050/%s.tif",
                               names(RCP85_2050_stack)),
            overwrite = TRUE)

writeRaster(RCP85_2100_stack,
            bylayer = TRUE,
            filename = sprintf("env_data/processed_files/RCP85_2100/%s.tif",
                               names(RCP85_2100_stack)),
            overwrite = TRUE)

