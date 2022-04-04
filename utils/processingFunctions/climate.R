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
  d3 <- getHeatDays(filePath = "data/heatDays/data_172346.csv", geometry)
  d4 <- getDrought(filePath = "data/drought/dm_export_20150101_20291231.csv", geometry, startYear =2016, endYear=2020)
  ## additional elements needed 
  index <- length(c(names(d1), names(d2),names(d3), names(d4)))-4
  
  dataframes <- list(d1,d2,d3,d4)
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