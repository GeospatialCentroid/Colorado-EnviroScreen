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
  cat("Ozone")
    d1 <- getOzone(filePath = "data/epa_cmaq/2017_ozone_daily_8hour_maximum.txt.gz" , geometry)
  cat("pm25")
    d2 <- getPM25(filePath = "data/epa_cmaq/2017_pm25_daily_average.txt.gz", geometry)
  cat("ejscreen")
    d3 <- ejscreen %>%
      dplyr::select("GEOID","leadPaint","deiselPM", "trafficeProx")
  cat("haps")
    d4 <- getHAPS(filePath = "data/haps/APENs 8_24_2021.csv" , geometry)
    #d5 <- getOtherHAPS(filePath = "data/haps/APENs 8_24_2021.csv" , geometry)
    
    # ## additional elements needed 
    index <- length(c(names(d1), names(d2),names(d3),names(d4)))-4
    
    dataframes <- list(d1,d2,d3,d4)
    # grab index based on the number of inputs 
    #index <- length(dataframes)
    df <- joinDataFrames(componentName = "Environmental_Exposures", dataframes)%>%
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
    df$envExp <- rowMeans(df[,((index+3):(index*2 + 2))])
    df <- df %>%
      dplyr::select(-component)
    return(df)
}