###
# process all the enviromental effects datasets
# carverd@colostate.edu
# 20211019
###


enviromentalEffects <- function(geometry, ejscreen){
  ### runs all processing functions and combines outputs into a single feature
  # geometry  : sf object used to define geogrpahic scale 
  # ejscreen : dataframe with GEOID and all elements pulled from the EJscreeen 
  ###
  
  # run functions 
  d1 <- ejscreen %>%
    dplyr::select("GEOID","waterDischarge","nplProx","rmpProx","tsdfProx")
  ## addataional elements needed 
  index <- length(names(d1))-1
  
  dataframes <- list(d1)
  df <- joinDataFrames(componentName = "Environmental_Effects", dataframes)%>%
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
  df$envEff <- rowMeans(df[,((index+3):(index*2 + 2))])
  df <- df %>%
    dplyr::select(-component)
  return(df)
}