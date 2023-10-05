#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-05
# What: Merge all the data sets
#----------------------------------------------------------------

# Merge all the data sets
# Use the source() function to call the R scripts that create the individual data sets 
# What variables would you use as keys to join the data?
# Why do you need to be careful when merging the armed conflict data and disaster data?

# Read in the original data

covariatedata <- read.csv(here("data", "original", "covariates.csv"), header = TRUE)

# Source the R scripts

source(here("scripts", "clean_mortality_data.R"))
source(here("scripts", "clean_disaster_data.R"))
source(here("scripts", "clean_armedconflict_data.R"))

# Put all data sets into a list

alllist <- list(covariatedata, mortalitydata, disasterdata, conflictdata)

# Merge all data sets in list

alllist |> reduce(left_join, by = c('ISO', 'year')) -> finaldata

# Replace NAs with 0's for armconflict, drought, earthquake, totaldeath

finaldata <- finaldata |>
  mutate(armconflict = replace_na(armconflict, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totaldeath = replace_na(totaldeath, 0))

# Explore data set 
  # size
  # column names
  # number of unique values in ISO

dim(finaldata)
names(finaldata)
length(unique(finaldata$ISO))

# Output results into different sub-folder

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)

# Push to github
