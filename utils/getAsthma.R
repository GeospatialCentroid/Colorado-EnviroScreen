###
# process the asthma data
# carverd@colostate.edu
# 20210915
###

#testing
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/asthma/Asthma_Hospitalization_Rate_(Census_Tracts).csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# tic()
# d2 <- getAsthma(filePath = filePath, geometry = geometry)
# toc()

getAsthma <- function(filePath, geometry){
  ### processes the adjusted asthma rate to various geometries
  # filePath : relative path to asthma dataset
  # geometry : SF feature of county, census tract or census block group
  ###
  x <- c("sf","dplyr", "stringr")
  lapply(x, require, character.only = TRUE)

  # process asthma Data
  d1 <- read.csv(filePath) %>%
    # set leading zero to match geoID in Geom object
    dplyr::mutate(GEOID = paste0("0",TRACT_FIPS))%>%
    dplyr::select(GEOID, ASTHMA_ADJRATE)

  ### select processing level by comparing length of GEOID between objects
  if(nchar(geometry$GEOID[1]) >= nchar(d1$GEOID[1])){
    #add tract-level column to use as join then keep original geoid (tract or block)
    geom <- as.data.frame(geometry) %>%
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 11)) %>%
      dplyr::left_join(d1, by = "GEOID") %>%
       dplyr::select(GEOID, asthma = ASTHMA_ADJRATE)
  }else{
    # when geometry is county level.. just cut FIPS to county level and group by that
    geom <-  d1 %>%
      dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 5)) %>%
      dplyr::group_by(GEOID) %>%
      dplyr::summarise(asthma = mean(ASTHMA_ADJRATE, na.rm = TRUE))
  }
  return(geom)
}
