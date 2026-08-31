#----------------------------------------------------------#
#
#
#                     Project name
#
#                  Render Exercises
#
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#

# Setup -----

library(here)

source(
  here::here("R/00_Config_file.R")
)

# Render -----
quarto::quarto_render(
  input = here::here("R/Exercises/01_example.qmd")
)
 