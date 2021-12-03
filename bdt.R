############################################################################################
#  bdt (Bootstrapped Difference Test)
############################################################################################

#  FUNCTION DESCRIPTION: This function provides a bootstrap method for contrasting values 
#                        between two treatments.

#  ARGUMENTS
# x: a factor vector containing the categories of the data values.
# y: a numeric vector of data values.
# rand: A number indicating the number of random samples for calculating the 
#       difference in data values between two categories.        
# two.tail: Logical. If TRUE, a two-tail difference test is calculated.
#           Otherwise, a one-tail difference test is calculated.

bdt <- function(x, y, ...) {
  
  y <- as.numeric(y)
  x <- as.factor( as.character(x) )
  
  stopifnot( length( levels(x) ) == 2 , all( table(x) > 0 ), is.numeric(y),
             length(x) == length(y))
  
  ext.args <- list(rand = 999, two.tail = TRUE)
  names.ext.args <- names(ext.args)
  ellips <- list(...)
  names.ellips <- names(ellips)
  ext.args[ names.ext.args %in% names.ellips ] <- 
    ellips[ names.ext.args[ names.ext.args %in% names.ellips ] ]

  g.mean <- unlist( by(y, x, median, simplify = FALSE) )
  g.lev <- names(g.mean)[ order(g.mean, decreasing = TRUE) ]
  g.n <- unlist( by(y, x, length, simplify = FALSE) )[g.lev]
  
  g1 <- y[ x == g.lev[1] ] 
  g2 <- y[ x == g.lev[2] ] 
  
  r1 <- sample(g1, ext.args[['rand']], replace = TRUE) 
  r2 <- sample(g2, ext.args[['rand']], replace = TRUE) 
  
  g.diff <- r1 - r2
  mean.g.diff <- mean(g.diff)
  p <- ( sum( g.diff <= 0 ) / length(g.diff) ) * ( ext.args[['two.tail']] + 1 )
  
  result <- list( 'statistic' = mean.g.diff, 'p' = p )
  
  return(result)
  
}

# Comment
