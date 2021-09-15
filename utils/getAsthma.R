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
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# t1 <- Sys.time()
# d2 <- getAsthma(filePath = filePath, geometry = geometry)
# t2 <- Sys.time() - t1
# t2


getAsthma <- function(filePath, geometry){
  ### processes the adjusted asthma rate to various geometries 
  # filePath : relative path to asthma dataset
  # geometry : SF feature of county, census tract or census block group
  ###
  
  # process asthma Data 
  d1 <- read.csv(filePath) %>%
    # set leading zero to match geoID in Geom object
    dplyr::mutate(GEOID = paste0("0",TRACT_FIPS))%>%
    dplyr::select(GEOID, ASTHMA_ADJRATE)
  
  # convert spatial Object to dataframe and assign storage value 
  geom <- as.data.frame(geometry)%>%
    dplyr::select("GEOID")%>%
    dplyr::mutate(asthma = NA)
  
  ### select processing level by comparing length of GEOID between objects
  if(nchar(geom$GEOID[1]) >= nchar(d1$GEOID[1])){ 
    # geom equal or smaller then input dataset only one value will transfer 
    for(i in seq_along(geom$GEOID)){
      # consider vectorizing this operation--slow with census block group data
      geom$asthma[i] <- d1$ASTHMA_ADJRATE[stringr::str_detect(geom$GEOID[i], d1$GEOID)]
    }
  }else{
    # condition to grab partial match from the smaller feature and summarize. 
    # not sure how to do this without the loop 
    for(i in seq_along(geom$GEOID)){
      val <- d1 %>%
        dplyr::filter(
          stringr::str_detect(GEOID,pattern = geom$GEOID[i]))%>%
        dplyr::summarise(val = median(ASTHMA_ADJRATE, na.rm = TRUE))
      # assign value back to geom feature
      geom$asthma[i]<- as.numeric(val$val)
    }
  }
  return(geom)
}
