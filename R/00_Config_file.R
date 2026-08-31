#----------------------------------------------------------#
#
#
#                     Project name
#
#                     Config file
#
#
#                O. Mottl, Author name
#                         2025
#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout
#   the whole repo. It loads packages, defines important variables,
#   authorises the user, and saves config file.

# Set the current environment
current_env <- environment()

# set seed
set.seed(1234)


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

if (
  isFALSE(
    exists("already_synch", envir = current_env)
  )
) {
  already_synch <- FALSE
}

if (
  isFALSE(already_synch)
) {
  library(here)
  # Synchronise the package versions
  renv::restore(
    lockfile = here::here("renv.lock")
  )
  already_synch <- TRUE

  # Save snapshot of package versions
  # renv::snapshot(lockfile =  "renv.lock")  # do only for update
}

# Define packages
package_list <-
  c(
    "fs",
    "here",
    "janitor",
    "jsonlite",
    "knitr",
    "languageserver",
    "quarto",
    "renv",
    "remotes",
    "rlang",
    "tidyverse",
    "usethis",
    "utils"
  )

# Attach all packages
sapply(package_list, library, character.only = TRUE)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed
current_dir <- here::here()


#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE
  )

# source them
if (
  length(fun_list) > 0
) {
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )
}


#----------------------------------------------------------#
# 4. Authorise the user -----
#----------------------------------------------------------#

# if applicable

#----------------------------------------------------------#
# 5. Define variables -----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 6. Graphical options -----
#----------------------------------------------------------#

source(
  here::here("R/generate_theme.R")
)

source(
  here::here("R/set_r_theme.R")
)
