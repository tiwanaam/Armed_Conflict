#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-05
# What: Derive armed conflict binary variable 
#----------------------------------------------------------------

# Read in the original data

conflict0 <- read.csv(here("data", "original", "conflictdata.csv"), header = TRUE)

# Derive the binary armed conflict variable
  # Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year

conflict0 %>%
  group_by(ISO, year) |>
  summarise(totaldeath = sum(best)) |>
  mutate(armconflict = ifelse(totaldeath < 25, 0, 1)) |>
  ungroup() |>
  mutate(year = year + 1) -> conflictdata

# Output results into different sub-folder

write.csv(conflictdata, here("data", "conflictdata.csv"), row.names = FALSE)