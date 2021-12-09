################################################################################
#  BootstrapingTests_EXAMPLE
################################################################################

#  FUNCTION DESCRIPTION: This is a simple guided example to run the  
#                        the bootstrapping tests. 



# IMPORTANT NOTE:
# Copy the files of the GitHub repository 'BootstrappingTests' 
# ('https://github.com/edurcunha/BootstrappingTests') into your 
# R project folder.


## TESTING DIFFERENCES BETWEEN GROUPS

# Loads the package
source('BootstrappingTests.R')

# Create a response variable
y <- c( rnorm(20, 3, 1), rnorm(20, 6, 1), rnorm(20, 12, 1) )   

# Create a categorical variable
x <- as.factor( rep( LETTERS[1:3], each = 20 ) )

# Plot the variables
boxplot(y ~ x)

# Run the bootstrapped difference test (bdt) for a pair of groups
difference.test <- bdt(x[x == "B" | x == "C"], y[x == "A" | x == "B"], 
                       rand = 9999, two.tail = TRUE )

# Explore the outputs
difference.test$statistic    # Gives the the mean difference between categories.
difference.test$p            # Gives the probability of not rejecting the null
                             # hypothesis.


# Run the bootstrapped difference test (bdt) for all pairwise combinations
# between groups

difference.tests <- bdtPairWise(x, y, 
                       rand = 9999, two.tail = TRUE )

difference.tests


## TESTING RELATIONSHIPS BETWEEN VARIABLES

# Create a explanatory variable
x <- rnorm(20)

# Create a response variable
y <- ( x * 2 ) + rnorm(20)   

# Plot the variables
plot(y ~ x)

# Run the bootstrapped regression test (brt) for the two variables
regression.test <- brt(x = x, y = y, rand = 9999 )

# Explore the outputs
regression.test$intercept    # Gives the one-sample bootstrapping test 
                             # for the intercept of the regression model fitted.

regression.test$slope        # Gives the one-sample bootstrapping test 
                             # for the intercept of the regression model fitted.

