# load packages
library(tidyverse)
library(here)



#  read data --------------------------------------------------------------

# read the dataset called "coral_binning.rds" from the data folder and 
# assign it the name "dat_cor"
# to read a rds file you can use the read_rds() function
# to navigate to the correct directory (folder "data") you can use the here()
# function within the read_rds call
dat_cor <- read_rds(here("data", "coral_binning.rds"))


# visualise data ----------------------------------------------------------

# take a look at the data
# each occurrence in the data has an age range with a maximum age (max_ma) and a
# minimum age (min_ma)
# create a plot with each genus on the y-axis and age on the x-axis, where the
# age range for each taxa is depicted with a line ranging from the minimum to 
# the maximum
# you can use the ggplot() function with geom_linerange()
ggplot(aes(xmin = max_ma, 
           xmax = min_ma, 
           y = genus), 
       data = dat_cor) +
  geom_linerange()



# bin occurrences  --------------------------------------------------------

# use 10 million year bins, starting at 0 Ma, and bin occurrences into 
# these bins

# first, take a look at what the maximum age is to know how many bins you need
max(dat_cor$max_ma)

# then set up a vector ranging from 0 Ma to the top of this maximum 
# in 10 Ma steps and name this vector "bins"
bins <- seq(0, 40, by = 10)

# now use the mutate() function in combination with the case_when() function to 
# create a new column called "bin
# if you are not familiar with case_when, type ?case_when() and read through 
# the help file
# within case_when, specify that occurrences which min_ma (minimum age) is 
# greater than or equal (>=) to the start of the first bin (bins[1], which is 0) 
# and which max_ma (maximum age) is less than or equal (<=) to the end of the 
# first bin (bins[2], which is 10), gets assigned the label "0-10". Use the & operator
# to combine your conditions within the case_when function. 
# Occurrences which min_ma (minimum age) is 
# greater than or equal (>=) to the start of the second bin (bins[2], which is 10) 
# and which max_ma (maximum age) is less than or equal (<=) to the end of the 
# second bin (bins[3], which is 20), gets assigned the label "10-20".
# ... same for all other bins
# use the .default setting within case_when to assign NA (NA_character_) to 
# all occurrences which cannot be binned
# assign this new dataframe to "dat_binned"
dat_binned <- dat_cor %>%
  mutate(
    bin = case_when(
      min_ma >= bins[1] & max_ma <= bins[2] ~ "0-10",
      min_ma >= bins[2] & max_ma <= bins[3] ~ "10-20",
      min_ma >= bins[3] & max_ma <= bins[4] ~ "20-30",
      min_ma >= bins[4] & max_ma <= bins[5] ~ "30-40",
      .default = NA_character_  # Discard occurrences that don't fit
    )
  ) 

# now use the filter() function to keep only those cases where bin is not NA
# for this, you can negate (!) the is.na() condition
# assign this to a dataframe called "dat_binned_clean"
dat_binned_clean <- filter(dat_binned, 
                           !is.na(bin))

# celebrate
### ᕕ( ಠ‿ಠ)ᕗ ###


# occurrences per bin -----------------------------------------------------

# calculate the occurrences per bin
# for this, use the count() function and assign this to the dataframe "dat_count"
dat_count <-  count(dat_binned_clean, bin)

# visualise this with ggplot and geom_col()
ggplot(data = dat_count) +
  geom_col(aes(bin, n))



# additional exercise -----------------------------------------------------

# make the two plots above visually appealing