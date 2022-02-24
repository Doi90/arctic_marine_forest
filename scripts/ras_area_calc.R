ras_area_calc <- function(ras,
                          lat25,
                          lat50,
                          lat75,
                          scenario){
  
  ## Total area
  
  total_area <- sum(ras[ ],
                    na.rm = TRUE)
  
  ## Top 75% latitude area
  
  mask1 <- st_as_sf(data.frame(x = 0,
                              y = 0), 
                   coords = 1:2, 
                   crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = lat75) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras1 <- mask(ras,
               mask1)
  
  above75 <- sum(ras1[ ],
                 na.rm = TRUE)

  ## Below 75% latitude area
  
  below75 <- total_area - above75
  
  ## Top 50% latitude area
  
  mask2 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = lat50) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras2 <- mask(ras,
               mask2)
  
  above50 <- sum(ras2[ ],
                 na.rm = TRUE)
  
  ## Below 50% latitude area
  
  below50 <- total_area - above50
  
  ## Top 25% latitude area
  
  mask3 <- st_as_sf(data.frame(x = 0,
                               y = 0), 
                    coords = 1:2, 
                    crs  = as.character(crs(ras))) %>% 
    st_buffer(dist = lat25) %>% 
    st_geometry() %>%
    st_as_sf()
  
  ras3 <- mask(ras,
               mask3)
  
  above25 <- sum(ras3[ ],
                 na.rm = TRUE)
  
  ## Below 25% latitude area
  
  below25 <- total_area - above25
  
  ## Output
  
  # Area correction 
  
  area_correction <- prod(res(ras))/1e06
  
  out <- data.frame(pred_scenario = scenario,
                    area_total = total_area * area_correction,
                    area_top_25 = above25 * area_correction,
                    area_top_50 = above50 * area_correction,
                    area_top_75 = above75 * area_correction,
                    area_bottom_25 = below75 * area_correction,
                    area_bottom_50 = below50 * area_correction,
                    area_bottom_75 = below25 * area_correction,
                    stringsAsFactors = FALSE)
  
  return(out)
  
}

