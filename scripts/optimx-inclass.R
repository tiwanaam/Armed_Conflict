#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-11-27
# What: optimx in-class
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

negll <- function(par){
  y <- dat$conflict18
  x1 <- dat$ever_eq
  x2 <- dat$ever_dr
  # 1. Calculate xbeta
  xbeta <- par[1] + par[2] * x1 + par[3] * x2
  # 2. Calculate p
  p <- exp(xbeta) / (1 + exp(xbeta))
  # 3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1 - y) * log(1 - p))
  return(val)
}

library(optimx)

opt <- optimx(
  par = c(0,0,0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

summary(opt, order = "convcode")

# Extract hessian matrix for BFGS optimization
hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se
  
# Compare these results to those obtained in glm 
model <- glm(conflict18 ~ ever_eq + ever_dr, data = dat, family = binomial)
summary(model)

# The results are similar, indicating that the optimization process has converged to a 
# point that is close to the maximum likelihood estimate obtained by the glm function.