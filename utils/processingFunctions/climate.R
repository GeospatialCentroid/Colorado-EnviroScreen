###
# process all the climate datasets
# carverd@colostate.edu
# 20211019
###


climate <- function(geometry){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- getWildfile(filePath = "data/wildfire/Data/whp2020_GeoTIF/whp2020_cnt_conus.tif" , geometry)
  d2 <- getFloodplain(filePath = "data/floodPlains/floodHazard.shp", geometry)
  ## addataional elements needed 
  
  dataframes <- list(d1,d2)
  df <- joinDataFrames(componentName = "Climate", dataframes)
  return(df)
}