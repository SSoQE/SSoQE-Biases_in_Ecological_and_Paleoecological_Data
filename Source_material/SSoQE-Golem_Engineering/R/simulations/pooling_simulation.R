# load libraries
library(here)
library(tidyverse)
library(lme4)

a_bar <- 1.5 #  α¯, the average log-odds of survival in the entire population of ponds
sigma <- 1.5 # σ, the standard deviation of the distribution of log-odds of survival among ponds
nstages <- 30 # number of stages
Ni <- rep(c(5, 20, 75) , each = 10) # number of observations in stages

# the values α¯ = 1.4 and σ = 1.5 define a Gaussian distribution of individual pond logodds of survival.
# so now we need to simulate all 60 of these intercept values from the implied
# Gaussian distribution with mean α¯ and standard deviation σ:
set.seed(24)
# α, a vector of individual stage intercepts, one for each stage
a_stage <- rnorm(nstages, mean = a_bar, sd = sigma)

# create a dataframe from these variables
dsim <- tibble(stage = factor(1:nstages),
               Ni = Ni ,
               true_a = a_stage)

# now we’re ready to simulate the binomial survival process. each stage i has ni potential survivors, 
# and nature flips each species coin, so to speak, with probability of survival pi
# this probability is defined by the logistic function
logistic <- function(alpha) {
  exp(alpha) / (1 + exp(alpha))
}

# putting the logistic into the random binomial function, we can generate a simulated
# survivor count for each stage
dsim$Si <- rbinom(nstages, prob = logistic(dsim$true_a), size = dsim$Ni)

# compute the no-pooling estimates from proportion of survivors to total observations
dsim$p_nopool <- dsim$Si / dsim$Ni

# the above is just a short cut of the following no-pooling model
mod_glm <- glm(cbind(Si, Ni - Si) ~ 0 + stage, 
                  family = "binomial", 
                  data = dsim)

# we actually check this by retransforming the coefficients which are transformed with the logit link
# we can use the plogis function to retransform them 
plogis(coef(mod_glm))

# compute the partial-pooling estimates
mod_glmm <- glmer(cbind(Si, Ni - Si) ~ 1 + (1 | stage), 
                   family = "binomial", 
                 data = dsim)

# these coefficients are transformed with the logit link
# we can use the plogis function to retransform them
coef_glmm <- coef(mod_glmm)$stage[[1]]
dsim$p_partpool <- plogis(coef_glmm)

# if we want to compare to the true per-stage survival probabilities used to generate the data,
# then we’ll also need to compute those, using the true_a column
dsim$p_true <- plogis(dsim$true_a)

# now we can compute the absolute error between the estimates and the true varying effects
dsim$No_Pooling <- abs(dsim$p_nopool - dsim$p_true)
dsim$Partial_Pooling <- abs(dsim$p_partpool - dsim$p_true)

# now we can plot it
plot1 <- dsim %>% 
  pivot_longer(cols = contains("p_"), 
               names_to = "pooling_type", 
               values_to = "prop_surv") %>% 
  filter(pooling_type != "p_true") %>% 
  left_join(tibble(pooling_type = c("p_nopool", "p_partpool"), 
                   pooling_label = c("No Pooling", "Partial Pooling"))) %>% 
  ggplot(aes(stage, prop_surv)) +
  geom_hline(yintercept = mean(dsim$p_true), 
             linetype = "dashed") +
  geom_vline(xintercept = c(9.5, 19.5, 30.5)) +
  geom_point(aes(fill = pooling_label), 
             shape = 21, 
             size = 3, 
             alpha = 0.7) +
  geom_text(aes(label = label), 
            data = tibble(stage = c(5, 15, 25), 
                          prop_surv = 0.05, 
                          label = c("Few Species (5)", 
                                    "Medium Species (20)", 
                                    "Many Species (75)"))) +
  scale_fill_manual(values = c("#C2A337", "#155560", "black")) +
  scale_x_discrete(breaks = seq(0, 30, by = 5)) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(fill = NULL, 
       y = "Proportion Survival", 
       x = "Stage") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line(), 
        legend.position = "top")

# save figure
ggsave(plot1, 
       filename = here("figures",
                       "simulation_glmm_1.png"), 
       width = 200, height = 120,
       units = "mm",
       bg = "white")

# same for the error
plot2 <- dsim %>%
  pivot_longer(cols = contains("Pooling"), 
               names_to = "error_type", 
               values_to = "abs_error") %>% 
  mutate(error_type = str_replace_all(error_type, "_", " ")) %>% 
  ggplot(aes(stage, abs_error)) +
  geom_vline(xintercept = c(9.5, 19.5, 30.5)) +
  geom_point(aes(fill = error_type), 
             shape = 21, 
             size = 3, 
             alpha = 0.7) +
  geom_point(aes(colour = error_type), 
             shape = "_",
             size = 20,
             data = . %>% 
               mutate(stage = as.integer(as.character(stage)), 
                      stg_type = case_when(stage <= 10 ~ "few", 
                                           between(stage, 10, 20) ~ "medium", 
                                           stage >=20 ~ "many")) %>% 
               group_by(error_type, stg_type) %>% 
               summarise(abs_error = mean(abs_error)) %>% 
               add_column(stage = rep(c(5, 25, 15), 2)), 
             show.legend = FALSE) +
  geom_text(aes(label = label), 
            data = tibble(stage = c(5, 15, 25), 
                          abs_error = 0.55, 
                          label = c("Few Species (5)", 
                                    "Medium Species (20)", 
                                    "Many Species (75)"))) +
  scale_fill_manual(values = c("#C2A337", "#155560")) +
  scale_colour_manual(values = c("#C2A337", "#155560")) +
  scale_x_discrete(breaks = seq(0, 30, by = 5)) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  labs(fill = NULL, 
       y = "Absolute Error", 
       x = "Stage") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.ticks = element_line(), 
        legend.position = c(0.8, 0.7))

# save figure
ggsave(plot2, 
       filename = here("figures",
                       "simulation_glmm_2.png"), 
       width = 200, height = 120,
       units = "mm",
       bg = "white")
