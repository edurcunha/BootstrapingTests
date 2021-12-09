############################################################################################
#  brt (Bootstrapped Regression Test)
############################################################################################

#  FUNCTION DESCRIPTION: This function provides a bootstrap method for a regression  
#                        between two variables.

#  ARGUMENTS
# x: a numeric vector of data values.
# y: a numeric vector of data values.
# rand: A number indicating the number of random samples for calculating the 
#       difference in data values between two categories.        

brt <- function(x, y, ...) {
  
  ext.args <- list(
    rand = 999
  )
  
  ellipsis <- list(...)
  args.repl <- names(ext.args) %in% names(ellipsis)
  ext.args[args.repl] <- ellipsis[ names(ext.args)[args.repl] ] 
  
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  x <- x[ !is.na(y) ]  
  y <- y[ !is.na(y) ]
  
  condition <- length(y) > 3 && length(x) == length(y)
  

  if(condition) {
    
    mod <- lm(y ~ x)
    obs.inter <- as.numeric( coef(mod)[1] )
    obs.slope <- as.numeric( coef(mod)[2] )
    
    
    rand.inter <- vector( length = ext.args[['rand']] )
    
    for(i in seq_len( ext.args[['rand']] ) ) {
      
      y.tmp <- sample(y)
      x.tmp <- sample(x)
      
      mod.tmp <- lm(y.tmp ~ x.tmp)
      rand.inter[i] <- as.numeric( coef(mod.tmp)[1] )

    }
    
    
    
    rand.slope <- vector( length = ext.args[['rand']] )
    
    for(i in seq_len( ext.args[['rand']] ) ) {
      
      y.tmp <- sample(y)
      x.tmp <- sample(x)
      
      mod.tmp <- lm(y.tmp ~ x.tmp)
      rand.slope[i] <- as.numeric( coef(mod.tmp)[2] )
      
    }

    
    result <- list(
      'intercept' = bost(obs = obs.inter, rands = rand.inter), 
      'slope' = bost(obs = obs.slope, rands = rand.slope)
    )

    
    return(result)
  
  }
  
}


