

socioEconomicFactors <- function(geometry, ejscreen, acsData, processingLevel){
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","peopleOfColor","highSchool")
  d2 <- acsData %>%
    dplyr::select("GEOID","percent_lowincome","percent_lingiso","percent_disability")
  d3 <- getHousingBurden(processingLevel, geometry, overWrite = FALSE)
  
  # there is not percent disability at census block group, so we need to pull in
  # from the census tract level data 
  if(processingLevel == "censusBlockGroup"){
    acs2 <- getACS(processingLevel = "censusTract", 2019, overwrite = FALSE)%>%
      dplyr::select(GEOID2 = GEOID, percent_disability)
    d2 <- d2 %>%
      mutate(GEOID2 = substr(GEOID, start = 1, stop = 11))%>%
      dplyr::left_join(acs2, by = c("GEOID2"))%>%
      dplyr::select("GEOID","percent_lowincome","percent_lingiso","percent_disability" ="percent_disability.y")
  }

  #combine datasets 
  dataframes <- list(d1,d2,d3)
  df <- joinDataFrames( dataframes)
  
  # determine the average value across all features 
  df$socEco <- df %>% 
    select(contains("_pcntl"))%>%
    apply(MARGIN = 1, FUN = gm_mean)
  
  return(df)
}
