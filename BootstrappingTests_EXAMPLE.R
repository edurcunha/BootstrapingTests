################################################################################
#  BootstrapingTests_EXAMPLE
################################################################################

#  FUNCTION DESCRIPTION: This is a simple guided example to run the  
#                        the bootstrapping tests. 



# IMPORTANT NOTE:
# Copy the files of the GitHub repository 'BootstrappingTests' 
# ('https://github.com/edurcunha/BootstrappingTests') into your 
# R project folder.

# Loads the package
source('BootstrappingTests.R')

# Create a response variable
y <- c( rnorm(20, 3, 1), rnorm(20, 6, 1) )   

# Create a categorical variable
x <- as.factor( rep( LETTERS[1:2], each = 20 ) )

# Run the bootstrapped difference test (bdt)
difference.test <- bdt(x, y, rand = 99999, two.tail = TRUE )
difference.test

# Explore the outputs
difference.test$statistic    # Gives the the mean difference between categories.
difference.test$p            # Gives the probability of not rejecting the null
                             # hypothesis.

