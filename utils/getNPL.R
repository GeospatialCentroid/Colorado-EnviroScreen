###
# process the NPL sites data 
# 20210907
# carverd@colostate.edu
###

# testing
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# d2 <- getNPL(geometry = geometry)

getNPL <- function(geometry){
  require(arcpullr, dplyr, sf)
  
  # grab dataset from arc server 
  # crs is ESPG:3857
  # need to convert equal area 6954 (https://spatialreference.org/ref/sr-org/6954/)
  npl <- arcpullr::get_spatial_layer(url = "https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/services/CDPHE_Colorado_Superfund_NPL_NRD/FeatureServer/0")%>%
    sf::st_transform(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  
  geom <- geometry %>%
    sf::st_transform(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))%>%
    dplyr::select(GEOID)
  
  # buffer each location using following conditions 
  # 250 : 1
  # 500 : 0.5
  # 750 : 0.25
  # 1000 : 0.1
  # distance in meters 
  # need to dissolve based on overlap. 
  
  
  ### testing for each NPL site, 
  # create column for each site 
  cNames <- rep(as.character(1:nrow(npl)))
  geom[,cNames] <- NA
  # loop over each site and test for intersections. 
  for(i in seq_along(npl$FID)){
    # select specific location
    site <- npl %>% 
      dplyr::slice(i)%>%
      select(FID)
    ###
    # workflow for buffering process 
    # buffer at minimun distance
    # if intersect, assign value and remove geom feature from options as this 
    # will be highest ranked score possible.
    # test for intersect at larger buffer, assign values or end test of no
    # intersection is found. 
    # repeat the filtering up until buffer dist == 1000
    
    # buffer and test interestion 
    t2 <- sf::st_buffer(x = site, dist = 250 )%>%
      sf::st_intersects(geom, sparse = FALSE)
    # define column index 
    index <- as.character(i)
    # assign value when possible and elemante from list. 
    if(length(unique(t2[1,]))==2){
      geom[t2 ,index] <- 1 
      g2 <- geom[is.na(geom[,index]),]
      # buffer to next distance 
      t3 <- sf::st_buffer(x = site, dist = 500 )%>%
        sf::st_intersects(g2, sparse = FALSE)
      #test for new match 
      if(length(unique(t3[1,]))==2){
        geom[t3 ,index] <- 0.5 
        g3 <- geom[is.na(geom[,index]),]
        # buffer to next distance 
        t4 <- sf::st_buffer(x = site, dist = 750 )%>%
          sf::st_intersects(g3, sparse = FALSE)
        if(length(unique(t4[1,]))==2){
          geom[t4 ,index] <- 0.25 
          g4 <- geom[is.na(geom[,index]),]
          # buffer to next distance 
          t5 <- sf::st_buffer(x = site, dist = 1000 )%>%
            sf::st_intersects(g4, sparse = FALSE)
          if(length(unique(t5[1,]))==2){
            geom[t5 ,index] <- 0.1
          }
        }
      }
    }
  }
    
    geom$nplScore <- geom %>% as.data.frame() %>% select(-c(1,2)) %>% rowSums(na.rm=TRUE) 
    geom <- geom %>%
      as.data.frame()%>%
      dplyr::select(GEOID, nplScore) 
    return(geom)
}



