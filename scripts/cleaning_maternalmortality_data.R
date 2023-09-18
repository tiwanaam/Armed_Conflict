#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-09-18
# What: Read in raw data, 
#         subset data, 
#         convert dataset into long format, 
#         write new clean data
#----------------------------------------------------------------

# BEGIN R PROJECT

# Read in original data set 

library(here)
here()

rawdat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)

# Subset the data to have only the variables Country.Name, X2000 – X2019

library(tidyverse)

cleandat <- rawdat %>%
  select(Country.Name, X2000:X2019)

# Convert the data set into a long format

cleandat <- cleandat %>%
  pivot_longer(cols = X2000:X2019, names_to = "Year", values_to = "MatMor") %>%
  mutate(Year = as.numeric(sub("^X", "", Year)))

head(cleandat, 20)
tail(cleandat, 20)

# Check if the year variable is stored as numeric 

print(is.numeric(cleandat$Year))

# PUSH TO GITHUB

library(usethis)

usethis::use_git() 
usethis::use_github()
