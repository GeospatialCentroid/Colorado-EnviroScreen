
getCensusAPIKey <- function(key){
  t1 <- tidycensus::census_api_key(key,
                                   install = TRUE)
  return(t1)
}

