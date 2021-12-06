############################################################################################
#  bdtPairWise (Bootstraped Difference Test)
############################################################################################

#  FUNCTION DESCRIPTION: This function provides a bootstrap method for comparisons between 
#                        pairwise treatments.

#  ARGUMENTS
# x: a factor vector containing the categories of data values.
# y: a numeric vector of data values.
# rand: A number indicating the number of random samples for calculating the 
#       difference in data values between two categories.        
# two.tail: Logical. If TRUE, a two-tail difference test is calculated.
#           Otherwise, a one-tail difference test is calculated.

bdtPairWise <- function(x, y, ...) {
  
  y <- as.numeric(y)
  x <- as.factor( as.character(x) )
  
  stopifnot( all( table(x) > 0 ), is.numeric(y), length(x) == length(y))
  
  ext.args <- list(
    rand = 99999, 
    two.tail = TRUE
  )
  
  ellipsis <- list(...)
  
  args.repl <- names(ext.args) %in% names(ellipsis)
  
  ext.args[args.repl] <- ellipsis[ names(ext.args)[args.repl] ] 
  
  
  
  labs <- levels(x)
  
  nlabs <- length(labs)
  
  result <- matrix(NA, ( ( (nlabs^ 2) / 2 ) - (nlabs / 2) ), 3, 
                   dimnames = list(NULL, c('Comparison', 'Statistic','p-value') ) )
  
  count <- 1
  
  for(i in 1:(nlabs-1)) {
    
    for(j in (i+1):nlabs) {
      
      x.tmp <- as.factor( as.character( x[ x == labs[i] | x == labs[j] ] ) )
      
      y.tmp <- y[ x == labs[i] | x == labs[j] ]
      
      bdt.tmp <- bdt(x = x.tmp, y = y.tmp, rand = ext.args[['rand']], 
                     two.tail = ext.args[['two.tail']] )
      
      result[count, 1] <- paste( c(labs[i], labs[j]), collapse = ' vs. ')
      
      result[count, 2:3 ] <- unlist(bdt.tmp)
      
      count <- count + 1
      
    } 
    
  }
  
  return(result)
  
}
