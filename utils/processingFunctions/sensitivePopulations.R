###
# process all the sensitive population datasets
# carverd@colostate.edu
# 20211019
###


sensitivePopulations <- function(geometry, ejscreen){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","under5","over64")
  d2 <- getHeartDisease(filePath = "data/heartDisease/Heart_Disease_in_Adults_-_CDPHE_Community_Level_Estimates_(Census_Tracts).csv", geometry)
  d3 <- getAsthma(filePath = "data/asthma/Asthma_Hospitalization_Rate_(Census_Tracts).csv",geometry)
  d4 <- getLifeExpectancy(filePath = "data/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv", geometry)
  d5 <- getLowBirthWeight(filePath = "data/lowWeightBirth/Low_Weight_Birth_Rate_(Census_Tracts).csv", geometry)
  ## additional elements needed 
  index <- length(c(names(d1), names(d2),names(d3),names(d4),names(d5)))-5
  
  ## to account for the positive nature of life expectancy
  d4$lifeExpectancy <- -1 * d4$lifeExpectancy
  
  
  # combine all dataframes
  dataframes <- list(d1,d2,d3,d4,d5)
  df <- joinDataFrames(componentName = "sensitive_populations", dataframes)%>%
    dplyr::mutate(
      across(where(is.numeric),
             .fns = list(pcntl = ~cume_dist(.)*100),
             .names = "{col}_{fn}")
    )

  df$lifeExpectancy <- -1 * df$lifeExpectancy
  
  # ### need to account for zero values due to NA values in indicators 
  # for(i in 3:(index+2)){
  #   n1 <- names(df)[i]
  #   n2 <- paste0(n1,"_pcntl")
  #   nas <- is.na(df[,n1])
  #   df[ ,n2][nas] <- NA
  #   
  # }
  
  # determine the average value across all features 
  df$senPop <- rowMeans(df[,((index+3):(index*2 + 2))], na.rm = TRUE)
  df <- df %>%
    dplyr::select(-component)
  return(df) 
}