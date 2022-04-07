

getWildfire <- function(filePath, geometry){
  # terra implementation 
  d1 <- terra::rast(filePath)
  
  # read in geometry features 
  g1 <- geometry %>%
    sf::st_transform(crs = terra::crs(d1))%>%
    dplyr::select(GEOID)
  
  # crop-
  d1 <- d1 %>%
    terra::crop(g1)
  
  # extract values 
  # assigning sf as spatvect for function, spatvect does not support same transformation as sf objects. 
  g2 <- terra::extract(d1, terra::vect(g1),fun = mean, na.rm = TRUE)
  # assign values to GEOID
  geom <- g1 %>%
    dplyr::mutate(
      wildfire = g2$whp2020_cnt_conus
    )%>%
    as.data.frame()%>%
    dplyr::select(GEOID, wildfire)
  
  return(geom) 
}


