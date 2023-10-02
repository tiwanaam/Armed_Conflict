#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-09-21
# What: Read in raw data, 
#         filter data,
#         subset data, 
#         create dummy variables and group data, 
#         write new clean data
#----------------------------------------------------------------

library(here)
here()

# Read in the raw data

rawdat <- read.csv(here("original", "disaster.csv"), header = TRUE)

library(tidyverse)

# Filter the data to include years 2000–2019 and disaster types "Earthquake" and "Drought"

cleandat <- rawdat %>%
  dplyr::filter(Year >= 2000 & Year <= 2019, Disaster.Type %in% c("Earthquake", "Drought"))

# Subset the data set to only include the following variables: Year, ISO, Disaster.type.

cleandat <- cleandat %>%
  select(Year, ISO, Disaster.Type)

# Create dummy variables 'drought' and 'earthquake'

cleandat <- cleandat %>%
  mutate(drought = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0))

# Print the first 10 rows of the resulting data frame

head(cleandat, 10)

# Group the data by 'Year' and 'ISO' and calculate the sum of 'drought' and 'earthquake' for each group

cleandat <- cleandat %>%
  group_by(Year, ISO) %>%
  summarize(drought = sum(drought), earthquake = sum(earthquake))

# Print the first 10 rows of the resulting summarized data frame

head(cleandat, 10)

# Output results into different sub-folder

write.csv(cleandat, here("data", "clean_disaster_data.csv"), row.names = FALSE)

# Push to github
