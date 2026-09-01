# load packages
library(tidyverse)
library(here)



#  read data --------------------------------------------------------------

# read the dataset called "species_distribution.rds" from the data folder and 
# assign it the name "dat_species"
# to read a rds file you can use the read_rds() function
# to navigate to the correct directory (folder "data") you can use the here()
# function within the read_rds call
dat_species <- read_rds(here("data", "species_distribution.rds"))



# your task ---------------------------------------------------------------

# explore the different sampling biases in the underlying data. 
# create nice visualisations and look at each bias from different angles. 
