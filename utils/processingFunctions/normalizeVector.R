###
# noramlize data function 
# 20211001
# carverd@colostate.edu
####


normalizeVector <- function(x){
  # normalizes a vector of numerical data
  max <- max(x, na.rm = TRUE)
  return(x / max)
}