#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-05
# What: cleaning mortality data sets
#----------------------------------------------------------------

# BEGIN R PROJECT

# Set working directory

library(here)
here() 

# Read in original data sets

matmor0 <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)
infantmor0 <- read.csv(here("data", "original", "infantmortality.csv"), header = TRUE)
neonatmor0 <- read.csv(here("data", "original", "neonatalmortality.csv"), header = TRUE)
under5mor0 <- read.csv(here("data", "original", "under5mortality.csv"), header = TRUE)

# Make a function to clean the data
  # subset data to have only the variables Country.Name and X2000 - X2019
  # convert data set from wide format into a long format 
    # select columns X2000 to X2019, change name of variable to year, remove prefix X, change values to mor
  # Make sure year variable is stored as numeric

cleanfun <- function(dataname, varname){
  dataname |>
    dplyr::select(Country.Name, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(Country.Name, year)
}

# Apply the function to each data set

matmor <- cleanfun(dataname = matmor0, varname = "matmor")
infantmor <- cleanfun(dataname = infantmor0, varname = "infantmor")
neonatmor <- cleanfun(dataname = neonatmor0, varname = "neonatmor")
under5mor <- cleanfun(dataname = under5mor0, varname = "under5mor")

# Put all data sets into a list

list <- list(matmor, infantmor, neonatmor, under5mor)

# Merge all data sets in list

list |> reduce(full_join, by = c('Country.Name', 'year')) -> mortalitydata

# Output results into different sub-folder

write.csv(mortalitydata, here("data", "mortalitydata.csv"), row.names = FALSE)

# Push to GitHub

# library(usethis)
# usethis::use_git_config(user.name = "tiwanaam", user.email = "amrit.tiwana@mail.utoronto.ca")
# Generate a git situation-report to confirm your user name and email
# usethis::git_sitrep()
# usethis::use_git() 
# usethis::use_github()