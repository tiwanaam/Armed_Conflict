#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-09-25
# What: Merge all the data sets
#----------------------------------------------------------------

# Merge all the data sets
# Use the source() function to call the R scripts that create the individual data sets 
# What variables would you use as keys to join the data?
# Why do you need to be careful when merging the armed conflict data and disaster data?

library(tidyverse)
library(here)

# Read in the covariate data

covariate_data <- read.csv(here("original", "covariates.csv"), header = TRUE)

# Change the variable name from "year" to "Year"

names(covariate_data)[names(covariate_data) == "year"] <- "Year"

# Source the R scripts

source(here("scripts", "cleaning_armedconflict_data.R"))
source(here("scripts", "cleaning_disaster_data.R"))
source(here("scripts", "cleaning_function.R"))

# Merge all data frames in list

alllist <- list(covariate_data, conflict_data, cleandat, merged_data)

alllist |> purrr::reduce(left_join, by = c('ISO', 'Year')) -> final_data

# Replace NA's with 0's

final_data <- final_data |>
  mutate(armed_conflict = replace_na(armed_conflict, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         best = replace_na(best, 0))

# Output results into different sub-folder

write.csv(final_data, file = here("data", "final_data.csv"), row.names = FALSE)

dim(final_data)
names(final_data)
length(unique(final_data$ISO))

# Push project to github
