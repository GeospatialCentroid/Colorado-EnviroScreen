###
# process heart disease dataset to various geomentries 
# carverd@colostate.edu
# 20210915
###

# use HeartDisease_Census_Tract_Estimate for census/blockgroup 
# use HeartDisease_County_Regional_Estimate for county 
# #testing
# library(sf)
# library(dplyr)
# library(stringr)
# filePath <- "data/heartDisease/Heart_Disease_in_Adults_-_CDPHE_Community_Level_Estimates_(Census_Tracts).csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# 
# t1 <- Sys.time()
# d2 <- getHearthDisease(filePath = filePath, geometry = geometry)
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
  
  
  # convert spatial Object to dataframe and assign storage value 
  geom <- as.data.frame(geometry)%>%
    dplyr::select("GEOID")%>%
    dplyr::mutate(heartDisease = NA)
  
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geom$GEOID[1]) >= nchar(d1$GEOID[1])){ 
    # geom equal or smaller then input dataset only one value will transfer 
    for(i in seq_along(geom$GEOID)){
      # consider vectorizing this operation--slow with census block group data
      geom$heartDisease[i] <- d1$HeartDisease_Census_Tract_Estimate[stringr::str_detect(geom$GEOID[i], d1$GEOID)]
    }
  }else{
    # condition to grab partial match from the smaller feature and summarize. 
    # not sure how to do this without the loop 
    for(i in seq_along(geom$GEOID)){
      val <- d1 %>%
        dplyr::filter(
          stringr::str_detect(GEOID,pattern = geom$GEOID[i]))
      # assign value back to geom feature
      geom$heartDisease[i]<- as.numeric(val$countyEstimate[1])
    }
  }
  return(geom)
}
