###
# process all the enviromental Exposures datasets
# carverd@colostate.edu
# 20211004
###


enviromentalExposures <- function(geometry, ejscreen,processingLevel){
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
    cat("other air Pollutants")
    d5 <- getOtherHAPS(filePath = "data/haps/APENs 8_24_2021.csv" , geometry,processingLevel = processingLevel,overWrite = FALSE)
    cat("drinking water")
    d6 <- getDrinkingWater(geometry)
    cat("noise")
    d7 <- getNoise(filePath = "data/noise/CONUS_L50dBA_sumDay_exi.tif", geometry)
    
    # ## additional elements needed 
    index <- length(c(names(d1), names(d2),names(d3),names(d4),names(d5), names(d6), names(d7)))-7
    
    dataframes <- list(d1,d2,d3,d4,d5,d6,d7)
    # grab index based on the number of inputs 
    df <- joinDataFrames(componentName = "Environmental_Exposures", dataframes)%>%
      dplyr::mutate(
        across(where(is.numeric),
               .fns = list(pcntl = ~cume_dist(.)*100),
               .names = "{col}_{fn}")
      )

    
    # determine the average value across all features 
    df$envExp <- rowMeans(df[,((index+3):(index*2 + 2))],na.rm = TRUE)
    df <- df %>%
      dplyr::select(-component)
    
    return(df)
}