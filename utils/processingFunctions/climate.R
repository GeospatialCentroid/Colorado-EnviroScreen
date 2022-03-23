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
  index <- length(c(names(d1), names(d2)))-2
  
  dataframes <- list(d1,d2)
  df <- joinDataFrames(componentName = "Climate", dataframes)%>%
    dplyr::mutate(
      across(where(is.numeric),
             .fns = list(pcntl = ~cume_dist(.)*100),
             .names = "{col}_{fn}")
    )

  
  # determine the average value across all features 
  df$climate <- rowMeans(df[,((index+3):(index*2 + 2))], na.rm = TRUE)
  df <- df %>%
    dplyr::select(-component)
  return(df)
}