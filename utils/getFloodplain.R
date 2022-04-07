

getFloodplain <- function(filePath, geometry){
  
  shp <- sf::st_read(filePath)%>%
    dplyr::filter(ZONE_SUBTY =="FLOODWAY")%>%
    sf::st_transform(crs = st_crs(geometry))
  
  # dataframe to hold information 
  geom <- geometry %>%
    dplyr::mutate(
      totalArea = sf::st_area(geometry),
      floodplainPercent = 0
    )%>%
    as.data.frame()%>%
    dplyr::select(GEOID, totalArea,floodplainPercent)
  
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
      geom$floodplainPercent[i] <- (t2 / geom$totalArea[i])*100 
    }
  }
  #calculate total area 
  geom <- geom %>%
    dplyr::select("GEOID","floodplainPercent")
  return(geom)
}
