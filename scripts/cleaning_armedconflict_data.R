#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-09-25
# What: Derive armed conflict 
#         create binary variable 
#----------------------------------------------------------------

library(tidyverse)

# Read the conflict data

conflict_data <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

# View the structure of the data

str(conflict_data)

# Change the variable name from "year" to "Year"

names(conflict_data)[names(conflict_data) == "year"] <- "Year"

# Create the binary armed conflict variable where 1 represents the presence of armed conflict and 0 represents the absence

conflict_data <- conflict_data %>%
  mutate(armed_conflict = ifelse(best > 0, 1, 0))

# View the first 20 rows of the modified data

head(conflict_data, 20)

# Save the new dataset with the binary variable

write.csv(conflict_data, here("data", "clean_conflict_data.csv"), row.names = FALSE)
