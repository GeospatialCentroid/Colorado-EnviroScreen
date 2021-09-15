# Get superfund sites

library(arcpullr)
library(sf)
library(dplyr)


getNPL <- function(geometry){
  
  #get data from arc server, fix geometry, and change to projected crs
  npl <-
    arcpullr::get_spatial_layer(url = "https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/services/CDPHE_Colorado_Superfund_NPL_NRD/FeatureServer/0") %>% 
    st_transform(crs = 5070) %>% 
    st_make_valid() 
  
  geom <- geometry %>%
    st_transform(crs = 5070) %>% 
    dplyr::select(GEOID)
  
  dist <- seq(250, 1000, by = 250)
  weight <- c(1, 0.5, 0.2, 0.1)
  intersect <- vector("list", length = length(dist))
  
  
  for (i in 1:length(dist)) {
    
    b <- st_buffer(npl, dist[i]) %>% 
      mutate(dist = dist[i], weight = weight[i])
    

    intersect[[i]] <- st_intersection(geom, b)
    

  }
  
  
  #now combine into one df
  intersect_all <- bind_rows(intersect)
  
  
  #now need to refine to distinct geoID/npl ID pairs, BUT if geoID overlaps
  # npl multiple times, take the HIGHEST weight, so arrange by weight first 
  # (distinct keeps first row of duplicated columns)
  
  npl_scores <- intersect_all %>% 
    arrange(desc(weight)) %>% 
    dplyr::distinct(GEOID, FID, .keep_all = TRUE) %>% 
    group_by(GEOID) %>% 
    summarise(npl_score = sum(weight, na.rm = TRUE)) %>%
    as.data.frame() %>%
    #right join so final dataset has all geoid, but non-intersects have NA
    right_join(as.data.frame(geom), by = "GEOID") %>% 
    dplyr::select(GEOID, npl_score) 
   
  
  return(npl_scores)

  
  
}


#test function

geometry <- st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
t <- Sys.time()
getNPL(geometry)
Sys.time() - t

#county = 2.98 sec
# tract = 5.25
# block = 9.38
