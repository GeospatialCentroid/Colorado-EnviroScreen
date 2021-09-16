###
# process heart disease dataset to various geomentries 
# carverd@colostate.edu
# 20210915
###

# use HeartDisease_Census_Tract_Estimate for census/blockgroup
# use HeartDisease_County_Regional_Estimate for county
#testing
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/heartDisease/Heart_Disease_in_Adults_-_CDPHE_Community_Level_Estimates_(Census_Tracts).csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# t1 <- Sys.time()
# d2 <- getHeartDisease(filePath = filePath, geometry = geometry)
# t2 <- Sys.time() - t1
# t2


getHeartDisease <- function(filePath, geometry){
  ### processes the adjusted asthma rate to various geometries 
  # filePath : relative path to asthma dataset
  # geometry : SF feature of county, census tract or census block group
  ###
  
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
  
  
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){ 
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry) %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 11)) %>% 
      left_join(d1, by = "GEOID") %>% dplyr::select(GEOID, heartDisease = HeartDisease_Census_Tract_Estimate) 
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>% 
      group_by(GEOID) %>% 
      summarise(heartDisease = countyEstimate[1] )
  }
  return(geom)
}
