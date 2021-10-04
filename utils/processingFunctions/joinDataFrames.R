###
# joins all datasets connected to a specific indicator 
# 20211004
# carverd@colostate.edu 
### 

### testing 
# componentName <- "exposures"
# dataframes <- list( need to add outputs from functions)
# tic()
# d2 <- joinDataFrames(componentName, dataframes)
# toc()
# View(d2)

joinDataFrames <- function(componentName, dataframes){
  ### combines the output data from specific components into a single dataframe
  # componentName: character describing the name of the component 
  # dataframes : list of all dataframes that contribute to the component 
  ###
  df <- dataframes %>% 
    purrr::reduce(dplyr::left_join, by = "GEOID")%>%
    dplyr::mutate(
      component = componentName
    )%>%
    dplyr::relocate(component, GEOID)  
  return(df)
}