#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-16
# What: Create descriptive table
#----------------------------------------------------------------

data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

data |>
  dplyr::select(country_name, ISO, year, matmor) |>
  dplyr::filter(year < 2018) |>
  arrange(ISO, year) |>
  group_by(ISO) |>
  mutate(diffmatmor = matmor - matmor[1L]) 

## Mortality trend for 2000-2017 by OECD

data |>
  ggplot(aes(x = year, y = matmor, group = ISO)) +
  geom_line(aes(color = as.factor(armconflict)), alpha = 0.5) +
  xlim(c(2000,2017)) +
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality", x = "Year", color = "Armed conflict") + 
  theme_bw()
