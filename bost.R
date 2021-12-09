############################################################################################
#  bost (Bootstrapped One Sample Test)
############################################################################################

#  FUNCTION DESCRIPTION: This function provides a bootstrap method for a one sample test  
#                        (observed value vs. numerical distribution of randomized values).

#  ARGUMENTS
# obs: a value for.
# rands: a numeric vector of data values.

bost <- function(obs, rands) {
  
  n.rand <- length( rands )
  
  comp <- list(obs, rands)
  
  if( sum( obs > rands ) < ( length( rands ) / 2 ) ) comp <- comp[2:1]
  
  dev <- comp[[1]] - comp[[2]]
  
  p <- ( sum( dev < 0 ) / n.rand ) * 2
  
  result <- list( 'statistic' = median(dev), 'p' = p )
  
  return(result)
  
}