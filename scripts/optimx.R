#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-11-26
# What: optimx
#----------------------------------------------------------------

library(here)
library(tidyverse)

data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

dat <- data %>%
  select(country_name, ISO, year, drought, earthquake, armconflict) %>% 
  group_by(country_name) %>% 
  mutate(conflict18 = as.numeric(any(armconflict == 1 & year == 2019))) %>%
  mutate(ever_eq = as.numeric(any(earthquake == 1 & year >= 2010 & year <= 2017))) %>%
  mutate(ever_dr = as.numeric(any(drought == 1 & year >= 2010 & year <= 2017))) %>%
  select(-armconflict, -drought, -earthquake, -year) %>%
  slice(1) %>%
  ungroup()

# Question 1 

table(dat$ever_eq)
table(dat$ever_dr)

# Question 2 

table(dat$ever_eq, dat$ever_dr)

# Question 3

dat[30, ]
# False

# Question 4

model <- glm(conflict18 ~ ever_eq + ever_dr, data = dat, family = binomial)
model
summary(model)
exp(coef(model))
exp(confint(model))

newdata <- data.frame(ever_eq = 0, ever_dr = 0)
predicted_probs <- predict(model, newdata = newdata, type = "response")
predicted_probs

# Question 5

# Based on this model, there is evidence to suggest that countries with at least 
# one earthquake and at least one drought between 2010 and 2017 are associated 
# with increased log odds of armed conflict exposure in 2018. The p-values for the
# coefficients of the variables are less than an alpha significance level of 0.05, 
# suggesting a statistically significant relationship.