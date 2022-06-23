#geometric mean
# take from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

gm_mean = function(x, na.rm=TRUE){
  # Drop the NA values from the list of features. 
  # the reduced the length of the denominator 
  x <- x[!is.na(x)]
  
  # exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  
  exp(mean(log(x[x>0])))
  
}

