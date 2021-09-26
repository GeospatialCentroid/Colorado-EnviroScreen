###
# function to grab tidycensus api key
# 20210831
# carverd@colostate.edu
###

getCensusAPIKey <- function(){
  ###
  # runs an api key and returns a binary for conditional testing
  require("tidycensus")
  t1 <- tidycensus::census_api_key("0ab15d4d7d8a87694979e5d5667502b365ae96f9")
  return(t1)
}
