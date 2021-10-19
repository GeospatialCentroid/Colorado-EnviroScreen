###
# process all the enviromental effects datasets
# carverd@colostate.edu
# 20211019
###


enviromentalEffects <- function(geometry, ejscreen){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","waterDischarge","nplProx","rmpProx","tsdfProx")
  ## addataional elements needed 
  
  dataframes <- list(d1)
  df <- joinDataFrames(componentName = "Environmental_Effects", dataframes)
  return(df)
}