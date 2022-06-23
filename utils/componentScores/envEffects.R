###
# process all the enviromental effects datasets
# carverd@colostate.edu
# 20211019
###


enviromentalEffects <- function(geometry, ejscreen, processingLevel){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","waterDischarge","nplProx","rmpProx","tsdfProx")
  d2 <- getSurfaceWater(filePath = "data/sufaceWater/Streams303dLayerFinal.shp", 
                        processingLevel = processingLevel, geometry,compile = FALSE, overWrite = FALSE)
  d3 <- getMines(geometry,processingLevel = processingLevel, overWrite = FALSE)
  d4 <- getProxyOilGas(geometry, processingLevel = processingLevel, overWrite = FALSE)
  
  ###Temp features for continued shiny work 
  d2 <- d2[!duplicated(d2$GEOID), ]
  d4 <- d4[!duplicated(d4$GEOID), ] 
  
  # combine datasets
  dataframes <- list(d1,d2,d3,d4)
  df <- joinDataFrames(dataframes)
  
  
  # determine the average value across all features 
  df$envEff <- df %>% 
    select(contains("_pcntl"))%>%
    apply(MARGIN = 1, FUN = gm_mean)
  
  return(df)
}