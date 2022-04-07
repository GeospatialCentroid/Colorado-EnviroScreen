

getHeartDisease <- function(filePath, geometry){

  # process asthma Data
  d1 <- read.csv(filePath) %>%
    # set leading zero to match geoID in Geom object
    dplyr::mutate(GEOID = paste0("0",Census_Tract_FIPS))%>%
    # grabbing the provide county percentage from dataset rather then calcualting
    tidyr::separate(col = HeartDisease_County_Regional_Estimate,
                    into = c("e1", "e2", "e3"),
                    sep = " ")%>%
    dplyr::mutate(countyEstimate = as.numeric(stringr::str_sub(e3, end = -2)))%>%
    dplyr::select(GEOID, HeartDisease_Census_Tract_Estimate, countyEstimate)


  # select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- st_drop_geometry(geometry) %>%
      dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) %>%
      dplyr::left_join(d1, by = c("GEOID2" = "GEOID")) %>%
      dplyr::select(GEOID, heartDisease = HeartDisease_Census_Tract_Estimate)
  }else{
  # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(heartDisease = countyEstimate[1] )
  }
  return(geom)
}


