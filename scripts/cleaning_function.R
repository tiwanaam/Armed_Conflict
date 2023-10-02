#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-09-25
# What: Read in raw data, 
#         subset data, 
#         convert dataset into long format, 
#         write new clean data,
#         create function to clean other datasets
#----------------------------------------------------------------

# Create a function that performs the same procedure on the maternal mortality data so that you  can apply the same steps on infant mortality, neonatal mortality, and under 5 mortality

library(here)
library(tidyverse)

cleaning_data <- function(rawdat, mortality){
  cleandat <- rawdat %>% select(Country.Name, X2000:X2019)
  cleandat <- cleandat %>% pivot_longer(cols = X2000:X2019, names_to = "Year", values_to = mortality) %>% mutate(Year = as.numeric(sub("^X", "", Year)))
  return(cleandat)
}

# Create new clean data set for maternal

maternal_mortality <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)
clean_maternal_mortality <- cleaning_data(maternal_mortality, "MatMort")

# Create new clean data set for infant

infant_mortality <- read.csv(here("original", "infantmortality.csv"), header = TRUE)
clean_infant_mortality <- cleaning_data(infant_mortality, "InfantMort")

# Create new clean data set for neonatal

neonatal_mortality <- read.csv(here("original", "neonatalmortality.csv"), header = TRUE)
clean_neonatal_mortality <- cleaning_data(neonatal_mortality, "NeoMort")

# Create new clean data set for under5

under5_mortality <- read.csv(here("original", "under5mortality.csv"), header = TRUE)
clean_under5_mortality <- cleaning_data(under5_mortality, "Under5Mort")

# Use the reduce() and full_join() functions to merge the four data sets to create one data set that contains the following variables: Country name, Year, Maternal mortality rate, Infant mortality rate, Neonatal mortality rate, Under 5 mortality rate

# Define a list of cleaned data frames

cleaned_data_list <- list(clean_maternal_mortality, clean_infant_mortality, clean_neonatal_mortality, clean_under5_mortality)

# Define the variables you want to keep from each data frame

variables <- c("Country.Name", "Year", "MatMort", "InfantMort", "NeoMort", "Under5Mort")

# Use reduce() to iteratively merge the data frames

merged_data <- reduce(cleaned_data_list, full_join, by = c("Country.Name", "Year"))

# Select only the desired variables

merged_data <- merged_data %>% select(all_of(variables))

# Print the first and last 10 rows

head(merged_data, 10)
tail(merged_data, 10)

# Use the countrycode() function in the countrycode package to add the ISO-3 country code variable to the new data set created in Step c. Call the new variable ISO. Then remove the Country name variable.

library(countrycode)

# specify the variable that includes the country names first
# use origin option to specify the format of the country names
# use destination to specify the format you want to convert the countries into

merged_data$ISO <- countrycode(merged_data$Country.Name, 
                               origin = "country.name", 
                               destination = "iso3c")
head(merged_data, 20)

# Remove the "Country.Name" variable from the data frame

merged_data <- merged_data %>% select(-Country.Name)

# Print the first few rows of the modified data frame

head(merged_data, 20)

# Output results into different sub-folder

write.csv(merged_data, here("data", "clean_merged_mortality_data.csv"), row.names = FALSE)