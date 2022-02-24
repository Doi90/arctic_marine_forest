ras_area_calc_latitudes <- function(ras,
                                    scenario){
  
  ## Total area
  
  total_area <- sum(ras[ ],
                    na.rm = TRUE)
  
  ## Above 40 degrees latitude area
  
  mask1 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = 5397733) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras1 <- mask(ras,
               mask1)
  
  above40 <- sum(ras1[ ],
                 na.rm = TRUE)
  
  ## Below 40 degrees latitude area
  
  below40 <- total_area - above40
  
  ## Above 50 degrees latitude area
  
  mask2 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = 4371229) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras2 <- mask(ras,
               mask2)
  
  above50 <- sum(ras2[ ],
                 na.rm = TRUE)
  
  ## Below 50 degrees latitude area
  
  below50 <- total_area - above50
  
  ## Above 60 degrees latitude area
  
  mask3 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = 3309820) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras3 <- mask(ras,
               mask3)
  
  above60 <- sum(ras3[ ],
                 na.rm = TRUE)
  
  ## Below 60 degrees latitude area
  
  below60 <- total_area - above60
  
  ## Above 70 degrees latitude area
  
  mask4 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = 2221671) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras4 <- mask(ras,
               mask4)
  
  above70 <- sum(ras4[ ],
                 na.rm = TRUE)
  
  ## Below 70 degrees latitude area
  
  below70 <- total_area - above70
  
  ## Above 80 degrees latitude area
  
  mask5 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = 1115409) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras5 <- mask(ras,
               mask5)
  
  above80 <- sum(ras5[ ],
                 na.rm = TRUE)
  
  ## Below 80 degrees latitude area
  
  below80 <- total_area - above80
  
  ## Output
  
  # Area correction 
  
  area_correction <- prod(res(ras))/1e06
  
  out <- data.frame(pred_scenario = scenario,
                    area_total = total_area * area_correction,
                    area_above_40 = above40 * area_correction,
                    area_above_50 = above50 * area_correction,
                    area_above_60 = above60 * area_correction,
                    area_above_70 = above70 * area_correction,
                    area_above_80 = above80 * area_correction,
                    area_below_40 = below40 * area_correction,
                    area_below_50 = below50 * area_correction,
                    area_below_60 = below60 * area_correction,
                    area_below_70 = below70 * area_correction,
                    area_below_80 = below80 * area_correction,
                    stringsAsFactors = FALSE)
  
  return(out)
  
}

