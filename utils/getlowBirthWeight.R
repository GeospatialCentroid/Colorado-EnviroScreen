###
# process the birth weight data 
# carverd@colostate.edu
# 20210916
### 



# Testing
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/lowWeightBirth/Low_Weight_Birth_Rate_(Census_Tracts).csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# t1 <- Sys.time()
# d2 <- getLowBirthWeight(filePath = filePath, geometry = geometry)
# t2 <- Sys.time() - t1
# t2


#use LWB_ADJRATE


getLowBirthWeight <- function(filePath, geometry){
  ### processes the adjusted asthma rate to various geometries 
  # filePath : relative path to asthma dataset
  # geometry : SF feature of county, census tract or census block group
  ###
  
  # process asthma Data 
  d1 <- read.csv(filePath) %>%
    # set leading zero to match geoID in Geom object
    dplyr::mutate(GEOID = paste0("0",TRACT_FIPS))%>%
    dplyr::select(GEOID, LWB_ADJRATE)
  
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){ 
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry) %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 11)) %>% 
      left_join(d1, by = "GEOID") %>% dplyr::select(GEOID, lowBirthWeight = LWB_ADJRATE) 
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>% mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>% 
      group_by(GEOID) %>% 
      summarise(lowBirthWeight = median(LWB_ADJRATE, na.rm = TRUE))
  }
  return(geom)
}


