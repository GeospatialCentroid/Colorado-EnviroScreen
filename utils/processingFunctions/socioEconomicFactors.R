###
# process all the sensitive population datasets
# carverd@colostate.edu
# 20211019
###


socioEconomicFactors <- function(geometry, ejscreen, acsData){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","peopleOfColor","highSchool")
  d2 <- acsData %>%
    dplyr::select("GEOID","percent_lowincome","percent_lingiso","percent_disability")
  ## additional elements needed 
  index <- length(c(names(d1), names(d2)))-2
  
  dataframes <- list(d1,d2)
  df <- joinDataFrames(componentName = "socioEconomic_Factors", dataframes)%>%
    dplyr::mutate(
      across(where(is.numeric),
             .fns = list(pcntl = ~ifelse(is.na(.), 0, cume_dist(.)*100)),
             .names = "{col}_{fn}")
      )
  # determine the average value across all features 
  df$socEco <- rowMeans(df[,((index+3):(index*2 + 2))])
  df <- df %>%
    dplyr::select(-component)
  return(df)
}