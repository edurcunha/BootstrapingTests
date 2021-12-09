############################################################################################
#  bdt (Bootstrapped Difference Test)
############################################################################################

#  FUNCTION DESCRIPTION: This function provides a bootstrap method for contrasting values 
#                        between two treatments.

#  ARGUMENTS
# x: a factor vector containing the categories of data values.
# y: a numeric vector of data values.
# rand: A number indicating the number of random samples for calculating the 
#       difference in data values between two categories.        
# two.tail: Logical. If TRUE, a two-tail difference test is calculated.
#           Otherwise, a one-tail difference test is calculated.

bdt <- function(x, y, ...) {
  
  x <- as.factor( as.character(x) )
  y <- as.numeric(y)
  
  x <- x[ !is.na(y) ]  
  y <- y[ !is.na(y) ]

  condition <- length( levels(x) ) == 2 && all( table(x) > 0 ) && 
    is.numeric(y) && length(x) == length(y)
  
  if(condition) {
    
    ext.args <- list(
      rand = 9999, 
      two.tail = TRUE
    )
    
    ellipsis <- list(...)
    
    args.repl <- names(ext.args) %in% names(ellipsis)
    
    ext.args[args.repl] <- ellipsis[ names(ext.args)[args.repl] ] 
    
    
    g.mean <- unlist( by(y, x, median, simplify = FALSE) )
    g.lev <- names(g.mean)[ order(g.mean, decreasing = TRUE) ]
    g.n <- unlist( by(y, x, length, simplify = FALSE) )[g.lev]
    
    g1 <- y[ x == g.lev[1] ] 
    g2 <- y[ x == g.lev[2] ] 
    
    # r1 <- sample(g1, ext.args[['rand']] * max(g.n), replace = TRUE) 
    # r2 <- sample(g2, ext.args[['rand']] * max(g.n), replace = TRUE) 
    
    r1 <- replicate( ext.args[['rand']],
                     sample(g1, max(g.n), replace = TRUE), 
                     simplify = F )
    r1 <- do.call(c, r1)
    
    r2 <- replicate( ext.args[['rand']],
                     sample(g2, max(g.n), replace = TRUE), 
                     simplify = F )
    r2 <- do.call(c, r2)
    
    g.diff <- r1 - r2
    mean.g.diff <- mean(g.diff)
    p <- ( sum( g.diff <= 0 ) / length(g.diff) ) * ( ext.args[['two.tail']] + 1 )
    p[ p > 1 ] <- 1
    
    result <- list( 'statistic' = mean.g.diff, 'p' = p )
    
  } else {
    
    result <- list( 'statistic' = NA, 'p' = NA )
    
  }
  
  return(result)
  
}

