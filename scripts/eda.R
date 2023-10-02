#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-02
# What: EDA
#----------------------------------------------------------------

source("../.Rprofile")

view(final_data)

# View the first few observations in the dataset

head(final_data)

# View the last view observations in the dataset 

tail(final_data)

# View a random selection of the data 

final_data |>
  slice_sample(n = 6)

# View the variables and their class 

final_data |>
  glimpse()

# Summary statistics of each variable 

final_data |>
  summary()

# Check for duplicates

install.packages("janitor")
library(janitor)

get_dupes(final_data)

# Look at the distribution of GDP

final_data |>
  ggplot(aes(x = GDP)) +
  geom_histogram(bins = 30)

final_data |>
  ggplot(aes(x = GDP)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

# Look at the distribution of population density 

final_data |>
  ggplot(aes(x = popdens)) +
  geom_histogram(bins = 30)

# Look at the relationship between GDP and mortality

final_data |>
  ggplot(aes(x = GDP, y = MatMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

final_data |>
  ggplot(aes(x = GDP, y = InfantMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

final_data |>
  ggplot(aes(x = GDP, y = NeoMort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

final_data |>
  ggplot(aes(x = GDP, y = Under5Mort)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Look at the relationship between Year and mortality

final_data |>
  ggplot(aes(x = Year, y = MatMort)) +
  geom_line() +
  labs(x = "Year", y = "Maternal Mortality Rate") +
  theme_minimal()

# Look at the relationship between GDP and armed conflict 

ggplot(final_data, aes(x = factor(armed_conflict), y = GDP)) +
  geom_boxplot() +
  labs(x = "Armed Conflict", y = "GDP") +
  theme_minimal() 

# Log transformation on GDP because the y-axis has a wide range of values

ggplot(final_data, aes(x = factor(armed_conflict), y = GDP)) +
  geom_boxplot() +
  labs(x = "Armed Conflict", y = "GDP (log scale)") +
  scale_y_log10() +
  theme_minimal() 

# Check to see the proportion of missing data 

mean(is.na(final_data)) # it is relatively low

# Summary

# There are 3,720 observations and 20 variables. There were no dupplicate observations found in the dataset. Approximately 18.92% of the countries in the dataset experienced armed conflict. The maternal mortality rate ranges from 2.0 to 2,480.0. The infant mortality rate ranges from 1.60 to 138.10. Neonatal mortality rates ranges from 0.80 to 60.90. The under-5 mortality rate ranges from 2.00 to 224.90. This suggests that mortality rates vary significantly in the dataset. On the log scales, the GDP variable looks normally distributed. Based on the scatterplots, different mortality rates decrease as GDP increases. Further examining maternal mortality, the rate decreases from 2000 to 2017. The boxplots examining the relationship between GDP and armed conflict overlap, indicate that countries with armed conflict and countries without armed conflict have different medians. The proportion of missingness is 0.00844 which is relatively low. I should look into what variables have missingingness and explore the type of missingness.
