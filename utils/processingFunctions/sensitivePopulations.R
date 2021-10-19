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
  
  dataframes <- list(d1,d2,d3,d4,d5)
  df <- joinDataFrames(componentName = "sensitive_populations", dataframes)
  return(df)
}