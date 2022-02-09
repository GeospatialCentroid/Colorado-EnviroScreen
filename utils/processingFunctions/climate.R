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
             .fns = list(pcntl = ~ifelse(is.na(.), 0, cume_dist(.)*100)),
             .names = "{col}_{fn}")
    )
  
  ### need to account for zero values due to NA values in indicators 
  for(i in 3:(index+2)){
    n1 <- names(df)[i]
    n2 <- paste0(n1,"_pcntl")
    nas <- is.na(df[,n1])
    df[ ,n2][nas] <- NA
    
  }
  
  # determine the average value across all features 
  df$climate <- rowMeans(df[,((index+3):(index*2 + 2))])
  df <- df %>%
    dplyr::select(-component)
  return(df)
}