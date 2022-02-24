.libPaths("/home/davidpw/R/lib/3.6")

library(raster)
library(sf)
library(MASS)
library(openxlsx)
library(tidyverse)
library(readxl)

points <- bind_rows(read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Agarum clathratum",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Delesseria sanginea",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Laminaria solidungula",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Himanthalia elongata",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Euthora cristata",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Dilsea socialis",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Odonthalia dentata",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")),
                    read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                              sheet = "Chondrus crispus",
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "text")))

points <- points %>%
  filter(!is.na(decimalLat) & !is.na(decimalLon))

points.sf <- sf::st_as_sf(points,
                          coords = c("decimalLon", "decimalLat"),
                          crs = 4326)

points.sf <- st_transform(points.sf,
                          crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# mapview(points.sf)

ras <- raster("env_data/processed_files/current/Present.Benthic.Min.Depth.Current.Velocity.Lt.min.tif.BOv2_1.tif")

points.ras <- rasterize(points.sf, ras, 1)
# mapview(points.ras, maxpixels = 10000000)

coords <- st_coordinates(points.sf)

kde <- MASS::kde2d(coords[ , 1], 
                   coords[ , 2], 
                   n = c(ncol(points.ras), nrow(points.ras)),
                   lims = c(-6414981, 6418099, -6427466, 6426544))

kde.ras <- raster(kde)
kde.ras <- setExtent(kde.ras,
                     points.ras)
crs(kde.ras) <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
# plot(kde.ras)

# kde_reproj <- projectRaster(kde.ras,
#                             crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

mask <- raster("env_data/mask/coastline_bathymetry_shoreline_mask.tif")

kde.ras <- crop(kde.ras,
                mask)

kde.mask <- mask(kde.ras,
                 mask)
kde.mask[kde.mask < 0] <- .Machine$double.eps

# plot(kde.mask)

writeRaster(kde.mask,
            "env_data/bias/kde_bias_layer.tif",
            overwrite = TRUE)
