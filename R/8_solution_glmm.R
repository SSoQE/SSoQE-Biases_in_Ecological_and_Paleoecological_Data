# now we can load it in accordance with the here package
library(here)
library(lme4)

# load in the data
dat_harnik <- read.csv(here("data", "Harnik2011_data.csv"))

# fit a generalized linear model with duration as the outcome and geo.range as the predictor/ exposure, 
# using the poisson family
mod_glm <- glm(duration ~ geo.range, 
              family = "poisson", 
              data = dat_harnik)

# we can use the coef function to extract the intercept and the coefficient of geo.range
# assign these two values to a variable called coef_glm
coef_glm <- coef(mod_glm)


# now fit a random intercept model with glmer, with duration as the outcome and 
# geo.range as the predictor/ exposure, 
# using the poisson family
# put the random intercept on Superfamily 
mod_glmm_int <- glmer(duration ~ geo.range + (1 | Superfamily ), 
                    family = "poisson",
                    data = dat_harnik)

# if you use the coef function, we will get the random effects. 
# take a look at it
coef(mod_glmm_int)

# you can extract the intercept and the overall coefficient of geo.range (both across random effects)
# with the fixef function. Assign this to a variable called coef_glmm_int and compare it with coef_glm
coef_glmm_int <- fixef(mod_glmm_int)


# now fit a random slope model with glmer, with duration as the outcome and 
# geo.range as the predictor/ exposure, 
# using the poisson family
# put the random slope of geo.range on Superfamily 
mod_glmm_slope <- glmer(duration ~ geo.range + (geo.range | Superfamily), 
                        family = "poisson", 
                        data = dat_harnik)

# if you use the coef function, we will get the random effects. 
# take a look at it
coef(mod_glmm_slope)

# you can extract the intercept and the overall coefficient of geo.range (both across random effects)
# with the fixef function. Assign this to a variable called coef_glmm_int and compare it with coef_glm
# and coef_glmm_int
coef_glmm_slope <- fixef(mod_glmm_slope)

# life coding goes here
