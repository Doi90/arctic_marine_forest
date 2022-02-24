#####################
### Load Packages ###
#####################

.libPaths("/home/davidpw/R/lib/3.6")

library(tidyverse)
library(sf)
library(raster)
library(mapview)

#################
### Coastline ###
#################

coast <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")

biggerBuf <- st_cast(coast, "MULTIPOLYGON") %>% 
  st_make_valid() %>% 
  st_buffer(dist = 15000 * (1 / 111000)) %>%  
  st_union() %>% 
  as_Spatial()

r <- raster("env_data/comment_rasters_raw/Present.Benthic.Min.Depth.Current.Velocity.Lt.min.tif.BOv2_1.tif")

r2 <- raster::mask(r, biggerBuf)
# plot(r2)
# mapview::mapview(r2)
p <- rasterToPoints(r2, spatial = TRUE) %>% 
  st_as_sf()
# plot(p)

buf10k <- st_cast(coast, "MULTIPOLYGON") %>% 
  st_make_valid() %>% 
  st_buffer(dist = 10000 * (1 / 111300)) %>%  
  st_union()

p2 <- st_crop(p, buf10k)
names(p2)[1] <- "distance"

distmap <- rasterize(p2, r, field = "distance")

# ext <- extent(c(-180, 180, 25, 90))
ext <- extent(c(-180, 180, -90, 90))

distmap <- crop(distmap,
                ext)

# plot(distmap, col = viridis::viridis(10))

##################
### Bathymetry ###
##################

bathymetry <- marmap::getNOAA.bathy(lon1 = -180,
                                    lon2 = 180,
                                    lat1 = 30,
                                    lat2 = 90,
                                    resolution = 5)#(10/1.852))

crop_ras_stack <- crop(r,
                       ext)

## Create Baythymetry-based Mask

bathy_ras <- marmap::as.raster(bathymetry)

bathy_ras <- raster::projectRaster(from = bathy_ras,
                                   to = crop_ras_stack)

bathy_ras[bathy_ras >= 0] <- NA

bathy_ras[bathy_ras <= -100] <- NA

bathy_ras[!is.na(bathy_ras)] <- 1


bathy_ras <- crop(bathy_ras,
                  ext)

bathy_ras <- resample(bathy_ras, 
                      distmap, 
                      method = "bilinear")

################
### Sediment ###
################

sediment <- st_read("env_data/sediment/final_coast_line.gpkg")

sediment <- st_cast(sediment,
                    to = "MULTILINESTRING")

Greenland_coast <- rnaturalearth::ne_countries(country = "Greenland", 
                                               scale = 10)

Greenland_coast <- st_as_sf(Greenland_coast) 

Greenland_coast <- Greenland_coast %>%
  dplyr::select(geometry) %>%
  mutate(VALUE = 1) %>%
  rename(geom = geometry) %>%
  st_buffer(dist = 10000 * (1 / 111300))

Greenland_coast <- st_transform(Greenland_coast,
                                crs = st_crs(sediment))

Greenland_coast <- st_cast(Greenland_coast,
                           to = "MULTILINESTRING")

sediment <- rbind(sediment, Greenland_coast)

sediment <- rasterize(sediment,
                      bathy_ras)

sediment[sediment > 0] <- 1

sediment <- crop(sediment,
                 ext)

##############################
### Merge into single mask ###
##############################

distmap[!is.na(distmap)] <- 1

mask <- calc(stack(bathy_ras,
                   distmap),
             fun = function(x) sum(x[1], x[2], na.rm = TRUE))       

# mask <- overlay(bathy_ras,
#                 distmap,
#                 fun = sum,
#                 na.rm = TRUE)

mask[mask == 0] <- NA
mask[mask > 1] <- 1

mask <- mask(mask,
             sediment)

mask <- raster::projectRaster(mask,
                              crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


# values(mask)[values(mask) > 1] <- 1

# mask_points <- rasterToPoints(mask,
#                               spatial = TRUE)
# mapview(mask_points)

writeRaster(mask, 
            "coastline_bathymetry_shoreline_mask.tif",
            overwrite = TRUE)

# a <- rasterToPoints(bathy_ras,
#                     spatial = TRUE)
# b <- rasterToPoints(distmap,
#                     spatial = TRUE)
# c <- rasterToPoints(sediment,
#                     spatial = TRUE)
# 
# mask_points <- st_union(st_as_sf(a),
#                         st_as_sf(b),
#                         st_as_sf(c))
# 
# mapview::mapview(mask_points)
# mapview::mapview(a)
# mapview::mapview(rasterToPoints(distmap,
#                                 spatial = TRUE))
# 
