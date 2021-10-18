###
# process floodplain dataset
# carverd@colostate.edu
# 20211018
###


# datasets was made available from CDPHE staff and represents an older
# FEMA flood plain layer. This is a create canidate for updating if possible.
# the layer "NFHL_08_20210805 S_FLD_HAZ_AR" was selected from the geodatabase 
# and exported as a shp. 

# filePath <- "data/floodPlains/floodHazard.shp"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# # 
# library(tictoc)
# tic()
# d2 <- getFloodplain(filePath = filePath, geometry = geometry)
# toc()
# 11.19 seconds on county, 
# 25.76 seconds on census tract
# 37.70 seconds on census tract
 

getFloodplain <- function(filePath, geometry){
  shp <- sf::st_read(filePath)%>%
    dplyr::filter(ZONE_SUBTY =="FLOODWAY")%>%
    sf::st_transform(crs = st_crs(geometry))
  
  # dataframe to hold information 
  geom <- geometry %>%
    dplyr::mutate(
      areaCounty = sf::st_area(geometry),
      floodplainPercent = 0
    )%>%
    as.data.frame()%>%
    dplyr::select(GEOID, areaCounty,floodplainPercent)
  
  # test for intersection with geometry 
  t1 <- sf::st_intersects(geometry, shp, sparse = TRUE)  
  # loop over each 
  for(i in seq_along(geometry$STATEFP)){
    #determine if intersection occurred. 
    if(length(t1[[i]])==0){
      geom$floodplainPercent[i] <- 0
    }else{
      #subset floodplain data based on overlap
      # clip to area county boundaries 
      f1 <- shp[t1[[i]], ]%>%
        sf::st_intersection(geometry[i, ])
      # calculate total area
      t2 <- sum(sf::st_area(f1))
      geom$floodplainPercent[i] <- (t2 / geom$areaCounty[i])*100 
    }
  }
  #calculate total area 
  geom <- geom %>%
    dplyr::select("GEOID","floodplainPercent")
  return(geom)
}
