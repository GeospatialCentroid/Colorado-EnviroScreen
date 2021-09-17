###
# process the ozone data  
# carverd@colostate.edu
# 20210903
###
library(dplyr)
library(sf)
library(data.table)

# path for testing the function 
# filePath <- "data/epa_cmaq/2017_ozone_daily_8hour_maximum.txt"
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")


getOzone <- function(filePath, geometry){
  require(dplyr, sf, data.table)
  # read in dataset - fread for large files
  d1 <- data.table::fread(filePath)
  
  # read in state of colorado geometry
  colorado <- sf::st_read("data/state/colorado.geojson")%>%
    sf::st_bbox()
  
  # filter by lat long from bbox of colorado 
  d2 <- d1 %>%
    #filter by lat long for quick spatial filter 
    dplyr::filter(Longitude >= colorado[1] & Longitude <= colorado[3] & 
                    Latitude >= colorado[2] & Latitude <= colorado[4]) %>%
    dplyr::group_by(FIPS)%>%
    dplyr::summarise(medainConc = median(`ozone_daily_8hour_maximum(ppb)`))%>%
    dplyr::mutate(
      # adding 0 to front to match format of geom objects
      FIPS = paste0("0",as.character(FIPS))
    )
  
  # replace when possilbe with case_when process 
  geom <- as.data.frame(geometry)%>%
    dplyr::select("GEOID")%>%
    dplyr::mutate(ozone = NA)
  ### need to develop different test for based on the input geometry. 
  ### probably could use the lenght of the GEOID relative to the length 
  ### of the FIPS. 
  if(nchar(geom$GEOID[1]) >= nchar(d2$FIPS[1])){ # no leading zero on FIPS so adding one
    for(i in 1:nrow(geom)){
      geom$ozone[i] <- d2$medainConc[str_detect(geom$GEOID[i], d2$FIPS)]
    }
  }else{
    # condition to grab partial match from the smaller feature and summarize. 
    # not sure how to do this without the loop 
    for(i in seq_along(geom$GEOID)){
      val <- d2 %>%
        dplyr::filter(
          stringr::str_detect(FIPS,pattern = geom$GEOID[i]))%>%
        dplyr::summarise(val = median(medainConc))
      # assign value back to geom feature
      geom$ozone[i]<- as.numeric(val$val)
    }
  }
  return(geom)
}






unique(geom$GEOID[1] %in% d2$FIPS)


# intesect with geometry feature 
geom <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")

## this should work with case_when it's just not coming together at the mement
# g1 <- geom %>%
#   dplyr::mutate(
#     ozone = case_when(
#       str_detect(GEOID, d2$FIPS) - d2$FIPS[str_detect(GEOID, d2$FIPS)]
#     )
#   )

# replace when possilbe with case_when process 
geom$ozone <- NA
for(i in 1:nrow(geom)){
  geom$ozone[i] <- d2$medainConc[str_detect(geom$GEOID[i], d2$FIPS)]
}
# subset to select features of interest 
geom1 <- geom %>%
  as.data.frame()%>%
  dplyr::select(GEOID, ozone)
