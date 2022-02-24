#####################
### Load Packages ###
#####################

.libPaths("/home/davidpw/R/lib/3.6")

library(raster)
library(usdm)

#########################
### Load Raster Files ###
#########################

files <- list.files("env_data/current/",
                    pattern = ".tif$",
                    full.names = TRUE)

ras_stack <- stack(files)

# plot(ras_stack)
# 
# ras_stack[[1]]

#################
### Load Mask ###
#################

mask <- raster("env_data/mask/coastline_bathymetry_shoreline_mask.tif")

##################
### Crop Stack ###
##################

ras_stack_crop <- crop(ras_stack,
                       extent(c(-180,180,30,90)))

########################
### Reproject Stacks ###
########################

ras_stack_reproj <- projectRaster(ras_stack_crop,
                                  crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

##########################
### Mask Raster Stacks ###
##########################

ras_stack_mask_reproj <- mask(ras_stack_reproj,
                              mask)

#############
### Tests ###
#############

corr_output <- vifcor(ras_stack_mask_reproj, 
                       th = 0.7, 
                       maxobservations = 10000)

vif_output <- vifstep(ras_stack_mask_reproj, 
                      th = 10, 
                      maxobservations = 10000)

saveRDS(corr_output,
        "outputs/raster_corr_test.rds")

saveRDS(vif_output,
        "outputs/raster_vif_test.rds")


vif_output@results$Variables[vif_output@results$Variables %in% corr_output@results$Variables]

corr_output@results$Variables
