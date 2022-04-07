
getLowBirthWeight <- function(filePath, geometry){

  
  # process birthweight data 
  d1 <- read.csv(filePath) %>%
    # set leading zero to match geoID in Geom object
    dplyr::mutate(GEOID = paste0("0",TRACT_FIPS))%>%
    dplyr::select(GEOID, LWB_ADJRATE)
  
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){ 
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- st_drop_geometry(geometry) %>% 
      mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) %>% 
      left_join(d1, by =  c("GEOID2" = "GEOID")) %>% 
      dplyr::select(GEOID, lowBirthWeight = LWB_ADJRATE) 
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>% 
      group_by(GEOID) %>% 
      summarise(lowBirthWeight = median(LWB_ADJRATE, na.rm = TRUE))
  }
  return(geom)
}


