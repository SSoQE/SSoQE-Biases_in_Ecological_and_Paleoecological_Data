library(MASS)
library(tidyverse)
library(here)
library(lme4)


# simulate data -----------------------------------------------------------


set.seed(5)
### build the g1
mu <- c(0, 0)
sigma <- rbind(c(1, -0.7), c(-0.7, 1))
g1 <- as.data.frame(mvrnorm(n = 200, mu = mu, Sigma = sigma))
g1$group <- c("Group 1")

### build the g2
mu <- c(3, 3)
sigma <- rbind(c(2, -0.7), c(-0.7, 2))
g2 <- as.data.frame(mvrnorm(n = 200, mu = mu, Sigma = sigma))
g2$group <- c("Group 2")

### build the g3
mu <- c(6, 6)
sigma <- rbind(c(3, -0.7), c(-0.7, 3))
g3 <- as.data.frame(mvrnorm(n = 200, mu = mu, Sigma = sigma))
g3$group <- c("Group 3")

# the combined data of all three groups
df <- rbind(g1, g2, g3) %>%
  as_tibble() %>% 
  rename(Outcome = V2, 
         Exposure = V1) %>% 
  add_column(subgroup = c(sample(c("A", "B", "C"), 200, replace = TRUE),
                          sample(c("D", "E", "F"), 200, replace = TRUE), 
                          sample(c("G", "H", "J"), 200, replace = TRUE)))


# visualise raw -----------------------------------------------------------


# create figure 1
plot_1 <- df %>%
  ggplot(aes(Exposure, Outcome)) + 
  geom_point(shape = 21, 
             size = 2, 
             stroke = 1, 
             colour = "#155560") +
  theme_minimal() +
  theme(legend.position = "none")

# save figure as png
ggsave(plot_1, 
       filename = here("figures",
                       "simpson_1.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")

# create figure 2
plot_2 <- df %>%
  ggplot(aes(Exposure, Outcome, 
             colour = group)) + 
  stat_ellipse(type = "norm") +
  geom_point(shape = 21, 
             size = 2, 
             stroke = 1) +
  scale_color_manual(values = c("#509A8E", "#C2A337", "#A1C3B3")) +
  theme_minimal() +
  theme(legend.position = "none")

# save figure as png
ggsave(plot_2, 
       filename = here("figures",
                       "simpson_2.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")


# save figure 4 as png
ggsave(plot_1 +
         geom_smooth(method = "lm",
                     se = FALSE,
                     colour = "#155560"), 
       filename = here("figures",
                       "simpson_3.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")


# random intercept --------------------------------------------------------

# fit
mod_int <- lmer(Outcome ~ Exposure + (1 | group), 
                data = df)
# predict
dat_int <- predict(mod_int) %>%
  as_tibble() %>% 
  bind_cols(df)

# plot and save
# save figure as png
ggsave(plot_2 +
         geom_line(aes(y = value), 
                   data = dat_int, 
                   linewidth = 1.6), 
       filename = here("figures",
                       "simpson_4.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")


# overall effect
ggsave(plot_2 +
         geom_line(aes(y = value), 
                   data = dat_int, 
                   linewidth = 1.6) +
         geom_abline(slope = fixef(mod_int)[2], 
                     intercept = fixef(mod_int)[1], 
                     linewidth = 2), 
       filename = here("figures",
                       "simpson_5.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")


# random slope --------------------------------------------------------

# fit
mod_slo <- lmer(Outcome ~ Exposure + (Exposure | group), 
                data = df)
# predict
dat_slo <- predict(mod_slo) %>%
  as_tibble() %>% 
  bind_cols(df)

# plot and save
# save figure as png
ggsave(plot_2 +
         geom_line(aes(y = value), 
                   data = dat_slo, 
                   linewidth = 1.6), 
       filename = here("figures",
                       "simpson_6.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")


# overall effect
ggsave(plot_2 +
         geom_line(aes(y = value), 
                   data = dat_slo, 
                   linewidth = 1.6) +
         geom_abline(slope = fixef(mod_slo)[2], 
                     intercept = fixef(mod_slo)[1], 
                     linewidth = 2), 
       filename = here("figures",
                       "simpson_7.png"), 
       width = 120, height = 120,
       units = "mm",
       bg = "white")
