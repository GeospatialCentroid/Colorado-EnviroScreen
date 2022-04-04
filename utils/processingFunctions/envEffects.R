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
  
  ## additional elements needed 
  index <- length(c(names(d1), names(d2), names(d3),names(d4)))-4
  
  dataframes <- list(d1,d2,d3,d4)
  df <- joinDataFrames(componentName = "Environmental_Effects", dataframes)%>%
    dplyr::mutate(
      across(where(is.numeric),
             .fns = list(pcntl = ~cume_dist(.)*100),
             .names = "{col}_{fn}")
    )
  
  
  # determine the average value across all features 
  df$envEff <- rowMeans(df[,((index+3):(index*2 + 2))], na.rm = TRUE)
  df <- df %>%
    dplyr::select(-component)
  return(df)
}