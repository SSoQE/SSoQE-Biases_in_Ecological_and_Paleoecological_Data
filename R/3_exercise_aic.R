# load the here library
library(here)

# load the diversity dataset located in the subfolder data/ and called "island_diversity.rds"
dat_div <- 
  
# build a simple linear model with diversity as the outcome and island size as the predictor 
mod1 <- 

# build a null model which assumes that there is no effect of size on diversity by means of an
# intercept-only linear model
mod2 <- 


# now fit a second intercept-only null model,
# a generalized linear model with a poisson distribution for count data
mod3 <- 

# use the coef function to get the estimated intercept of the GLM
# is the model giving us the same estimate as the mean of diversity?
 

# we obviously have to transform the coefficient back into our real world
# find out what link function the poisson model is using by looking at the help description of family()
?family()

# retransform the estimated coefficient by using the reverse link and
# compare the estimate for the intercept with the mean of diversity



# now fit a fourth model, 
# a generalised linear model with diversity as the outcome and size as the predictor
mod4 <- 


# take a look at the AIC function
?AIC

# compare all four models, which is the best performing one?
AIC()
