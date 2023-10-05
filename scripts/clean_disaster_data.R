#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-05
# What: cleaning disaster data set
#----------------------------------------------------------------

# Set working directory

library(here)
here()

# Read in the original data

disaster0 <- read.csv(here("data", "original", "disaster.csv"), header = TRUE)

# Clean the data
  # subset to only include years 2000 - 2019
  # subset to only include types "Earthquake" and "Drought"
  # subset to only include the variables: Year, ISO, Distaster.Type
  # create dummy variables drought and earthquake
  # create data set where only one row of observations exists for each country and each year

disaster0 |>
  dplyr::filter(Year >= 2000 & Year <= 2019) |>
  dplyr::filter(Disaster.Type %in% c("Earthquake", "Drought")) |>
  dplyr::select(Year, ISO, Disaster.Type) |>
  rename(year = Year) |>
  group_by(year, ISO) |>
  mutate(drought0 = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake0 = ifelse(Disaster.Type == "Earthquake", 1, 0)) |>
  summarize(drought = max(drought0),
            earthquake = max(earthquake0)) |> 
  ungroup() -> disasterdata

# Output results into different sub-folder

write.csv(disasterdata, here("data", "disasterdata.csv"), row.names = FALSE)

# Push to github