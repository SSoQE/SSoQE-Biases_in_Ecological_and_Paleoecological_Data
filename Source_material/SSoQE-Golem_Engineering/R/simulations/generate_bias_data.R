# load packages 
library(tidyverse)
library(here)



# 10 fake species
species <- paste0("species_", LETTERS[1:10])

# create biased data 
# set seed for reproducibility
set.seed(42)

# strong sampling bias in europe & north america
# europe, longitude 10, lat 50
dat_eur <- tibble(longitude = rnorm(150,  10, 5), 
                  latitude = rnorm(150, 50, 5)) 

# north america, longitude -100, lat 40
dat_na  <- tibble(longitude = rnorm(100,  -100, 5), 
                  latitude = rnorm(100, 40, 5))  

# a few scattered globally (underrepresented regions)
dat_other <- tibble(
  longitude = runif(50, -180, 180),
  latitude  = runif(50, -60, 70)
)

# combine all and assign random species
dat_occ <- bind_rows(dat_eur, dat_na, dat_other) %>%
  mutate(species = sample(species, n(), replace = TRUE))

# add biome and elevation
dat_occ <- dat_occ %>% 
  add_column(biome = sample(c("Aquatic", "Grassland", "Forest", "Desert", "Tundra"), 
                            300, 
                            replace = TRUE, 
                            prob = c(0.1, 0.5, 0.2, 0.1, 0.1)),  # biome biases
             elevation = sample(1:4000, 300, 
                                replace = TRUE, 
                                prob = c(rep(0.7/2000, 2000), 
                                         rep(0.3/2000, 2000))))  %>% # more occurrences at low elevations
  select(species, longitude, latitude, species, biome, elevation)


# save 
write_rds(dat_occ, here("data", "species_distribution.rds"))

