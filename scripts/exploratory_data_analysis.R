#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-02
# What: EDA
#----------------------------------------------------------------

source("./.Rprofile")

view(finaldata)

# View the first few observations in the dataset

head(finaldata)

# View the last view observations in the dataset 

tail(finaldata)

# View a random selection of the data 

finaldata |>
  slice_sample(n = 6)

# View the variables and their class 

finaldata |>
  glimpse()

# Summary statistics of each variable 

finaldata |>
  summary()

# Check for duplicates

install.packages("janitor")
library(janitor)

get_dupes(finaldata)

# Look at the distribution of GDP

finaldata |>
  ggplot(aes(x = GDP)) +
  geom_histogram(bins = 30)

finaldata |>
  ggplot(aes(x = GDP)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

# Look at the distribution of population density 

finaldata |>
  ggplot(aes(x = popdens)) +
  geom_histogram(bins = 30)

# Look at the relationship between GDP and mortality

finaldata |>
  ggplot(aes(x = GDP, y = MatMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

finaldata |>
  ggplot(aes(x = GDP, y = InfantMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

finaldata |>
  ggplot(aes(x = GDP, y = NeoMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

finaldata |>
  ggplot(aes(x = GDP, y = Under5Mort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Look at the relationship between Year and mortality

finaldata |>
  ggplot(aes(x = Year, y = MatMort)) +
  geom_line() +
  labs(x = "Year", y = "Maternal Mortality Rate") +
  theme_minimal()

# Look at the relationship between GDP and armed conflict 

ggplot(finaldata, aes(x = factor(armed_conflict), y = GDP)) +
  geom_boxplot() +
  labs(x = "Armed Conflict", y = "GDP") +
  theme_minimal() 

# Log transformation on GDP because the y-axis has a wide range of values

ggplot(finaldata, aes(x = factor(armed_conflict), y = GDP)) +
  geom_boxplot() +
  labs(x = "Armed Conflict", y = "GDP (log scale)") +
  scale_y_log10() +
  theme_minimal() 

# Check to see the proportion of missing data 

mean(is.na(finaldata)) # it is relatively low

# Summary

# There are 3,720 observations and 20 variables. There were no duplicate observations found in the data set. Approximately 18.92% of the countries in the data set experienced armed conflict. The maternal mortality rate ranges from 2.0 to 2,480.0. The infant mortality rate ranges from 1.60 to 138.10. Neonatal mortality rates ranges from 0.80 to 60.90. The under-5 mortality rate ranges from 2.00 to 224.90. This suggests that mortality rates vary significantly in the data set. On the log scales, the GDP variable looks normally distributed. Based on the scatter plots, mortality rates decrease as GDP increases. Further examining maternal mortality, the rate decreases from 2000 to 2017. The box plots examining the relationship between GDP and armed conflict overlap, indicating that countries with armed conflict and countries without armed conflict do not have different medians. The proportion of missingness is 0.00844 which is relatively low. I should look into what variables have missingingness and explore the type of missingness.
