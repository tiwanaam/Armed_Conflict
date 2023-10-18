#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-16
# What: Create descriptive figure
#----------------------------------------------------------------

data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

forfigure <- data |>
  dplyr::select(country_name, ISO, year, matmor) |>
  dplyr::filter(year < 2018) |>
  group_by(ISO) |>
  mutate(diffmatmor = matmor - matmor[1L]) |>
  arrange(ISO, desc(year)) |>
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0 , 1, 0)) |>
  arrange(ISO, year) |>
  ungroup() |>
  dplyr::filter(incmatmor == 1)
length(unique(forfigure$ISO))

fig1 <- forfigure |>
  ggplot(aes(x = year, y = matmor, group = ISO)) +
  geom_line(aes(color = country_name), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 10 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  theme_bw(base_size = 12)

ggsave(fig1, file = here("figures", "fig1_increasingmaternalmortality.png"), width = 8, height = 5)
