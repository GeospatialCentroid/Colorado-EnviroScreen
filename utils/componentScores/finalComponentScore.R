

finalComponentScore <- function(dataframes){
  
  df <- dataframes %>%
    purrr::reduce(dplyr::left_join, by = "GEOID")%>%
    rowwise() %>%
    dplyr::mutate(
      pollClimBurden =  sum(envExp, (envEff * 0.5) , (climate *0.5),na.rm=TRUE)/3,
      popCharacteristic = sum(senPop, socEco, na.rm = TRUE)/2,
      finalScore = sum(pollClimBurden, `popCharacteristic`, na.rm = TRUE)
    )
  ### Error ###
  # percent_rank was returning NaN values within the mutate... Pulled out for short fix
  df$envExp_Pctl <- cume_dist(df$envExp)*100
  df$envEff_Pctl <- cume_dist(df$envEff)*100
  df$climate_Pctl <- cume_dist(df$climate)*100
  df$senPop_Pctl <- cume_dist(df$senPop)*100
  df$socEco_Pctl <- cume_dist(df$socEco)*100
  df$pollClimBurden_Pctl <- cume_dist(df$pollClimBurden) *100
  df$popCharacteristic_Pctl <- cume_dist(df$popCharacteristic) *100
  df$finalScore_Pctl <- cume_dist(df$finalScore) *100
  
  return(df)
}