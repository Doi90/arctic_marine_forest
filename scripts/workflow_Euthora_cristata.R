#####################
### Load Packages ###
#####################

.libPaths("/home/davidpw/R/lib/3.6")

library(raster)
library(sf)
library(bushfireSOS)
library(tidyverse)
library(openxlsx)
library(readxl)
library(blockCV)

#########################
### Load Species Data ###
#########################

species <- "Euthora cristata"

spp_data <- read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
                      sheet = species,
                      col_types = c("text", "text", "text", "text", "numeric", "numeric", "text"))

spp_data$decimalLat <- as.numeric(spp_data$decimalLat)

spp_data$decimalLon <- as.numeric(spp_data$decimalLon)

spp_data <- spp_data %>%
  # filter(!is.na(decimalLat) & !is.na(decimalLon)) %>%
  drop_na(decimalLat,
          decimalLon) %>%
  mutate(Species = species,
         decimalLat = as.numeric(decimalLat),
         decimalLon = as.numeric(decimalLon)) %>%
  filter(decimalLat > 30)

spp_data.sf <- sf::st_as_sf(spp_data,
                            coords = c("decimalLon", "decimalLat"),
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#reproject to Australian Albers
spp_data.sf <- sf::st_transform(spp_data.sf,
                                crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

spp_data[ ,c("Long", "Lat")] <- sf::st_coordinates(spp_data.sf)

table(spp_data$Species)

spp_data$Value <- 1

spp_data <- spp_data %>%
  dplyr::select(Species,
                Long,
                Lat,
                Value)

#########################
### Background Points ###
#########################

bg <- bind_rows(read_xlsx("spp_data/Occurence_high_lat_seaweeds_2021_22xi21_master.xlsx",
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

bg_df <- data.frame(Species = species,
                    Long = bg$decimalLon,
                    Lat = bg$decimalLat,
                    Value = 0,
                    stringsAsFactors = FALSE)

bg_df <- bg_df %>%
  # filter(!is.na(decimalLat) & !is.na(decimalLon)) %>%
  drop_na(Long,
          Lat) %>%
  mutate(Species = species,
         Long = as.numeric(Long),
         Lat = as.numeric(Lat)) %>%
  filter(Lat > 30)

bg.sf <- sf::st_as_sf(bg_df,
                      coords = c("Long", "Lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#reproject to Australian Albers
bg.sf <- sf::st_transform(bg.sf,
                          crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

bg_df[ , c("Long", "Lat")] <- sf::st_coordinates(bg.sf)

###############################
### Load Environmental Data ###
###############################

current_stack <- stack(list.files("env_data/processed_files/current/",
                                  full.names = TRUE))

RCP26_2050_stack <- stack(list.files("env_data/processed_files/RCP26_2050/",
                                     full.names = TRUE))

RCP26_2100_stack <- stack(list.files("env_data/processed_files/RCP26_2100/",
                                     full.names = TRUE))

RCP45_2050_stack <- stack(list.files("env_data/processed_files/RCP45_2050/",
                                     full.names = TRUE))

RCP45_2100_stack <- stack(list.files("env_data/processed_files/RCP45_2100/",
                                     full.names = TRUE))

RCP60_2050_stack <- stack(list.files("env_data/processed_files/RCP60_2050/",
                                     full.names = TRUE))

RCP60_2100_stack <- stack(list.files("env_data/processed_files/RCP60_2100/",
                                     full.names = TRUE))

RCP85_2050_stack <- stack(list.files("env_data/processed_files/RCP85_2050/",
                                     full.names = TRUE))

RCP85_2100_stack <- stack(list.files("env_data/processed_files/RCP85_2100/",
                                     full.names = TRUE))

mask <- raster("env_data/mask/coastline_bathymetry_shoreline_mask.tif")

#######################
### Pixel Filtering ###
#######################

## Presences

samplecellID <- raster::cellFromXY(current_stack,
                                   as.data.frame(spp_data[ , c("Long", "Lat")]))

dup <- duplicated(samplecellID)

spp_data <- spp_data[!dup, ]

## Background

samplecellID <- raster::cellFromXY(current_stack,
                                   as.data.frame(bg_df[ , c("Long", "Lat")]))

dup <- duplicated(samplecellID)

bg_df <- bg_df[!dup, ]

#################################
### Create Single Data Object ###
#################################

comb_data <- rbind(spp_data,
                   bg_df)

############################
### Distribution Summary ###
############################

max_lat <- min(abs(spp_data$Lat))  # max latitude is the minimum value because north pole is 0

min_lat <- max(abs(spp_data$Lat))

range_lat <- abs(max_lat - min_lat)

top_25_lat <- abs(max_lat + (0.25 * range_lat))

mid_lat <- abs(max_lat + (0.5 * range_lat))

bottom_25_lat <- abs(max_lat + (0.75 * range_lat))

##################################
### Extract Environmental Data ###
##################################

env_extract <- raster::extract(current_stack,
                               comb_data[ , c("Long", "Lat")])

comb_data <- cbind(comb_data,
                   env_extract)

comb_data <- comb_data[complete.cases(comb_data), ]

#####################
### Model Fitting ###
#####################

source("scripts/fit_pres_bg_model.R")
source("scripts/cross_validate.R")
source("scripts/regularisedMaxent.R")

model_eval <- cross_validate(spp_data = list(data = comb_data),
                             type = "po",
                             k = 5,
                             # parallel = FALSE,
                             spatial_cv = TRUE,
                             filepath = sprintf("outputs/maxent_cv/%s",
                                                gsub(" ", "_", species)))

saveRDS(model_eval,
        sprintf("outputs/model_eval/model_eval_%s.rds",
                gsub(" ", "_", species)))

model <- fit_pres_bg_model(spp_data = list(data = comb_data),
                           tuneParam = TRUE,
                           k = 5,
                           filepath = sprintf("outputs/maxent/%s",
                                              gsub(" ", "_", species)))

saveRDS(model,
        sprintf("outputs/models/model_%s.rds",
                gsub(" ", "_", species)))

##################
### Prediction ###
##################

current_pred <- predict(current_stack,
                        model = model)

current_pred_thresh <- predict_threshold(pred_ras = current_pred,
                                         threshold = model_eval[3])

RCP26_2050_pred <- predict(RCP26_2050_stack,
                           model = model)

RCP26_2050_pred_thresh <- predict_threshold(pred_ras = RCP26_2050_pred,
                                            threshold = model_eval[3])

RCP26_2100_pred <- predict(RCP26_2100_stack,
                           model = model)

RCP26_2100_pred_thresh <- predict_threshold(pred_ras = RCP26_2100_pred,
                                            threshold = model_eval[3])

RCP45_2050_pred <- predict(RCP45_2050_stack,
                           model = model)

RCP45_2050_pred_thresh <- predict_threshold(pred_ras = RCP45_2050_pred,
                                            threshold = model_eval[3])

RCP45_2100_pred <- predict(RCP45_2100_stack,
                           model = model)

RCP45_2100_pred_thresh <- predict_threshold(pred_ras = RCP45_2100_pred,
                                            threshold = model_eval[3])

RCP60_2050_pred <- predict(RCP60_2050_stack,
                           model = model)

RCP60_2050_pred_thresh <- predict_threshold(pred_ras = RCP60_2050_pred,
                                            threshold = model_eval[3])

RCP60_2100_pred <- predict(RCP60_2100_stack,
                           model = model)

RCP60_2100_pred_thresh <- predict_threshold(pred_ras = RCP60_2100_pred,
                                            threshold = model_eval[3])

RCP85_2050_pred <- predict(RCP85_2050_stack,
                           model = model)

RCP85_2050_pred_thresh <- predict_threshold(pred_ras = RCP85_2050_pred,
                                            threshold = model_eval[3])

RCP85_2100_pred <- predict(RCP85_2100_stack,
                           model = model)

RCP85_2100_pred_thresh <- predict_threshold(pred_ras = RCP85_2100_pred,
                                            threshold = model_eval[3])

## Write to file

writeRaster(current_pred,
            sprintf("outputs/predictions/%s_current.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP26_2050_pred,
            sprintf("outputs/predictions/%s_RCP26_2050.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP26_2100_pred,
            sprintf("outputs/predictions/%s_RCP26_2100.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP45_2050_pred,
            sprintf("outputs/predictions/%s_RCP45_2050.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP45_2100_pred,
            sprintf("outputs/predictions/%s_RCP45_2100.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP60_2050_pred,
            sprintf("outputs/predictions/%s_RCP60_2050.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP60_2100_pred,
            sprintf("outputs/predictions/%s_RCP60_2100.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP85_2050_pred,
            sprintf("outputs/predictions/%s_RCP85_2050.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP85_2100_pred,
            sprintf("outputs/predictions/%s_RCP85_2100.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(current_pred_thresh,
            sprintf("outputs/predictions/%s_current_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP26_2050_pred_thresh,
            sprintf("outputs/predictions/%s_RCP26_2050_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP26_2100_pred_thresh,
            sprintf("outputs/predictions/%s_RCP26_2100_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP45_2050_pred_thresh,
            sprintf("outputs/predictions/%s_RCP45_2050_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP45_2100_pred_thresh,
            sprintf("outputs/predictions/%s_RCP45_2100_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP60_2050_pred_thresh,
            sprintf("outputs/predictions/%s_RCP60_2050_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP60_2100_pred_thresh,
            sprintf("outputs/predictions/%s_RCP60_2100_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP85_2050_pred_thresh,
            sprintf("outputs/predictions/%s_RCP85_2050_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

writeRaster(RCP85_2100_pred_thresh,
            sprintf("outputs/predictions/%s_RCP85_2100_thresh.tif",
                    gsub(" ", "_", species)), 
            overwrite = TRUE)

####################################
### Prediction Area Calculations ###
####################################

source("scripts/ras_area_calc.R")
source("scripts/ras_area_calc_latitudes.R")

area_df <- rbind(ras_area_calc(current_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "current"),
                 ras_area_calc(RCP26_2050_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP26_2050"),
                 ras_area_calc(RCP26_2100_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP26_2100"),
                 ras_area_calc(RCP45_2050_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP45_2050"),
                 ras_area_calc(RCP45_2100_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP45_2100"),
                 ras_area_calc(RCP60_2050_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP60_2050"),
                 ras_area_calc(RCP60_2100_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP60_2100"),
                 ras_area_calc(RCP85_2050_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP85_2050"),
                 ras_area_calc(RCP85_2100_pred,
                               lat25 = top_25_lat,
                               lat50 = mid_lat,
                               lat75 = bottom_25_lat,
                               scenario = "RCP85_2100"))

area_df_thresh <- rbind(ras_area_calc(current_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "current_thresh"),
                        ras_area_calc(RCP26_2050_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP26_2050_thresh"),
                        ras_area_calc(RCP26_2100_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP26_2100_thresh"),
                        ras_area_calc(RCP45_2050_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP45_2050_thresh"),
                        ras_area_calc(RCP45_2100_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP45_2100_thresh"),
                        ras_area_calc(RCP60_2050_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP60_2050_thresh"),
                        ras_area_calc(RCP60_2100_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP60_2100_thresh"),
                        ras_area_calc(RCP85_2050_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP85_2050_thresh"),
                        ras_area_calc(RCP85_2100_pred_thresh,
                                      lat25 = top_25_lat,
                                      lat50 = mid_lat,
                                      lat75 = bottom_25_lat,
                                      scenario = "RCP85_2100_thresh"))

area_df_lat <- rbind(ras_area_calc_latitudes(current_pred,
                                             scenario = "current"),
                     ras_area_calc_latitudes(RCP26_2050_pred,
                                             scenario = "RCP26_2050"),
                     ras_area_calc_latitudes(RCP26_2100_pred,
                                             scenario = "RCP26_2100"),
                     ras_area_calc_latitudes(RCP45_2050_pred,
                                             scenario = "RCP45_2050"),
                     ras_area_calc_latitudes(RCP45_2100_pred,
                                             scenario = "RCP45_2100"),
                     ras_area_calc_latitudes(RCP60_2050_pred,
                                             scenario = "RCP60_2050"),
                     ras_area_calc_latitudes(RCP60_2100_pred,
                                             scenario = "RCP60_2100"),
                     ras_area_calc_latitudes(RCP85_2050_pred,
                                             scenario = "RCP85_2050"),
                     ras_area_calc_latitudes(RCP85_2100_pred,
                                             scenario = "RCP85_2100"))

area_df_lat_thresh <- rbind(ras_area_calc_latitudes(current_pred_thresh,
                                                    scenario = "current_thresh"),
                            ras_area_calc_latitudes(RCP26_2050_pred_thresh,
                                                    scenario = "RCP26_2050_thresh"),
                            ras_area_calc_latitudes(RCP26_2100_pred_thresh,
                                                    scenario = "RCP26_2100_thresh"),
                            ras_area_calc_latitudes(RCP45_2050_pred_thresh,
                                                    scenario = "RCP45_2050_thresh"),
                            ras_area_calc_latitudes(RCP45_2100_pred_thresh,
                                                    scenario = "RCP45_2100_thresh"),
                            ras_area_calc_latitudes(RCP60_2050_pred_thresh,
                                                    scenario = "RCP60_2050_thresh"),
                            ras_area_calc_latitudes(RCP60_2100_pred_thresh,
                                                    scenario = "RCP60_2100_thresh"),
                            ras_area_calc_latitudes(RCP85_2050_pred_thresh,
                                                    scenario = "RCP85_2050_thresh"),
                            ras_area_calc_latitudes(RCP85_2100_pred_thresh,
                                                    scenario = "RCP85_2100_thresh"))

write.csv(area_df,
          sprintf("outputs/area_calculations/area_calc_%s.csv",
                  gsub(" ", "_", species)))

write.csv(area_df_thresh,
          sprintf("outputs/area_calculations/area_calc_thresh_%s.csv",
                  gsub(" ", "_", species)))

write.csv(area_df_lat,
          sprintf("outputs/area_calculations/area_calc_latitudes_%s.csv",
                  gsub(" ", "_", species)))

write.csv(area_df_lat_thresh,
          sprintf("outputs/area_calculations/area_calc_latidudes_thresh_%s.csv",
                  gsub(" ", "_", species)))
