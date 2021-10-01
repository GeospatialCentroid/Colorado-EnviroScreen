###
# process the NPL sites  
# carverd@colostate.edu
# 20210826
###



# ### testing
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# #
# # library(tictoc)
# tic()
# d2 <- getNPL(geometry = geometry)
# toc()
# View(d2)



getNPL <- function(geometry){
  ### Takes in NPL sites and processes them 
  # geometry : sf object representing spatial scale 
  ###
  
  x <- c("sf","dplyr", "arcpullr")
  lapply(x, require, character.only = TRUE)
  
  
  #get data from arc server, fix geometry, and change to projected crs
  npl <-
    arcpullr::get_spatial_layer(url = "https://services3.arcgis.com/66aUo8zsujfVXRIT/arcgis/rest/services/CDPHE_Colorado_Superfund_NPL_NRD/FeatureServer/0") %>% 
    st_transform(crs = 5070) %>% 
    st_make_valid() 
  # reproject data to the espg:5070
  geom <- geometry %>%
    st_transform(crs = 5070) %>% 
    dplyr::select(GEOID)

  # read in the buffer function *** we will remove this but it's here for testing
  source("utils/processingFunctions/bufferObjects.R")
  
  # running buffering process. 
  b1 <- bufferObjects(bufferFeature = npl, 
                      geometry = geom, 
                      dist = seq(250, 1000, by = 250),
                      weight = c(1, 0.5, 0.2, 0.1)
                      )
  
  #now need to refine to distinct geoID/bufferFeature ID pairs, BUT if geoID overlaps
  # bufferFeature multiple times, take the HIGHEST weight, so arrange by weight first 
  # (distinct keeps first row of duplicated columns)
  
  bufferFeature_scores <- b1 %>% 
    arrange(desc(weight)) %>% # ensures highest score is kept
    dplyr::distinct(GEOID, FID, .keep_all = TRUE) %>% 
    group_by(GEOID) %>% 
    summarise(bufferFeature_score = sum(weight, na.rm = TRUE)) %>%
    as.data.frame() %>%
    #right join so final dataset has all geoid, but non-intersects have NA
    right_join(as.data.frame(geometry), by = "GEOID") %>% 
    dplyr::select(GEOID, bufferFeature_score) 
  
  # output resulting df  
  return(bufferFeature_scores)
  
}
