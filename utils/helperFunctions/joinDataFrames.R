

joinDataFrames <- function(dataframes){
  ### combines the output data from specific components into a single dataframe
  # componentName: character describing the name of the component 
  # dataframes : list of all dataframes that contribute to the component 
  ###
  df <- dataframes %>% 
    purrr::reduce(dplyr::left_join, by = "GEOID")%>%
    dplyr::mutate(
      across(where(is.numeric),
             .fns = list(pcntl = ~cume_dist(.)*100),
             .names = "{col}_{fn}")
    )
}