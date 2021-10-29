###
# process all the enviromental Exposures datasets
# carverd@colostate.edu
# 20211004
###


enviromentalExposures <- function(geometry, ejscreen){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
    d1 <- getOzone(filePath = "data/epa_cmaq/2017_ozone_daily_8hour_maximum.txt.gz" , geometry)
    d2 <- getPM25(filePath = "data/epa_cmaq/2017_pm25_daily_average.txt.gz", geometry)
    d3 <- ejscreen %>%
      dplyr::select("GEOID","leadPaint","deiselPM", "trafficeProx")
    d4 <- getHAPS(filePath = "data/haps/APENs 8_24_2021.csv" , geometry)
    d5 <- getOtherHAPS(filePath = "data/haps/APENs 8_24_2021.csv" , geometry)
    ## addataional elements needed 
    index <- length(c(names(d1), names(d2),names(d3),names(d4),names(d5)))-5
    
    dataframes <- list(d1,d2,d3,d4,d5)
    df <- joinDataFrames(componentName = "Environmental_Exposures", dataframes)%>%
      dplyr::mutate(
        across(where(is.numeric),
               .fns = list(pcntl = ~ifelse(is.na(.), 0, cume_dist(.)*100)),
               .names = "{col}_{fn}")
      )
    
    # determine the average value across all features 
    df$envExp <- rowMeans(df[,((index+3):(index*2 + 2))])
    df <- df %>%
      dplyr::select(-component)
    return(df)
    
    
    return(df)
}