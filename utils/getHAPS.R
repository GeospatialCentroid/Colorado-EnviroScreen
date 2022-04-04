###
# process the haps dataset
# carverd@colostate.edu
# 20210826
###


### testing
# filePath <- "data/haps/APENs 8_24_2021.csv"
# geometry <- sf::st_read("data/censusBlockGroup/coloradoCensusBlockGroups.geojson")
# geometry <- sf::st_read("data/censusTract/coloradoCensusTracts.geojson")
# geometry <- sf::st_read("data/county/coloradoCounties.geojson")
# #
# # library(tictoc)
# tic()
# d2 <- processHAPS( filePath = filePath, geometry = geometry)
# toc()
# View(d2)
# county : 11.3
# censusTract : 23 sec
# censusBlockgroup : 36 sec


getHAPS <- function(filePath, geometry){
  ### Processes the HAPS dataset establishing a volume weighted score by geometry
  # filePath : relative path to csv of HAPS data
  # geometry : sf object of the census block group, census track, or county
  # return : a dataframe with geoid and haps score 
  ###

  x <- c("sf","dplyr")
  lapply(x, require, character.only = TRUE)
  
  #call in normalize function 
  source("utils/processingFunctions/normalizeVector.R")
  
  # read in dataset and drop locations with no coordinates 
  d1 <- read.csv(filePath)%>%
    dplyr::filter(SITE_X_COORDINATE != 0)
  
  ### Generate 5 year median values for all pollutants 
  # select columns of interest 
  d2 <- d1 %>% 
    dplyr::select(APCD_SITE_ID                ,SITE_100414_ESTIM
                  ,SITE_106467_ESTIM                ,SITE_106990_ESTIM
                  ,SITE_18540299_ESTIM                ,SITE_50000_ESTIM
                  ,SITE_56235_ESTIM                ,SITE_71432_ESTIM
                  ,SITE_75070_ESTIM                ,SITE_75218_ESTIM
                  ,SITE_7782505_ESTIM                ,SITE_822060_ESTIM
                  ,SITE_91203_ESTIM                ,SITE_ASC_ESTIM
                  ,SITE_CE_ESTIM
                  )%>%
    dplyr::group_by(APCD_SITE_ID)%>%
    dplyr::summarise_all(mean ,na.rm = TRUE)
  
  ### normalize data based on volume of emission 
  d2[,2:15] <- apply(d2[,2:15], MARGIN = 2, FUN = normalizeVector)
  ### calculate total 
  d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)
  ### drop all non poluting sites 
  d2 <- d2[d2$total != 0, ]
  

  ### create spatial feature based on sites of interest 
  sp1 <- d1 %>%
    dplyr::filter(APCD_SITE_ID %in% d2$APCD_SITE_ID)%>%
    distinct(APCD_SITE_ID, .keep_all = TRUE)%>%
    dplyr::select("APCD_SITE_ID",   # rename for input into buffer process
                  "SITE_X_COORDINATE",
                  "SITE_Y_COORDINATE")%>%
    st_as_sf(.,coords=c("SITE_X_COORDINATE","SITE_Y_COORDINATE"),crs=4269)%>%
    sf::st_transform(crs = 5070) %>% 
    st_make_valid() 
  
  # reproject data to the espg:5070
  geom <- geometry %>%
    st_transform(crs = 5070) %>% 
    dplyr::select(GEOID)
  
  # read in the buffer function *** we will remove this but it's here for testing
  source("utils/processingFunctions/bufferObjects.R")
  
  # running buffering process. 
  b1 <- bufferObjects(bufferFeature = sp1, 
                      geometry = geom, 
                      dist = seq(250, 1000, by = 250),
                      weight = c(1, 0.5, 0.2, 0.1)
  )
  
  # select total score 
  d3 <- d2 %>%
    select(APCD_SITE_ID, total)
  
  # select the top score value only, combine with site score, and summarize
  bufferFeature_scores <- b1 %>% 
    arrange(desc(weight)) %>% # ensures highest score is kept
    dplyr::distinct(GEOID, APCD_SITE_ID, .keep_all = TRUE) %>%
    dplyr::left_join(y = d3, by = "APCD_SITE_ID")%>%
    dplyr::mutate(value = weight * total)%>%
    group_by(GEOID) %>% 
    summarise(bufferFeature_score = sum(value, na.rm = TRUE)) %>%
    as.data.frame() %>%
    dplyr::select(GEOID, bufferFeature_score) 
  
  # join back to geometry object to get full list of features 
  
  
  
  ### attached GEOID to all points 
  geom <- geometry %>%
    dplyr::select(GEOID)%>%
    dplyr::left_join(y = bufferFeature_scores,by = "GEOID")%>%
    as.data.frame()%>%
    dplyr::select(GEOID, HAPS = bufferFeature_score)

  return(geom)
}
