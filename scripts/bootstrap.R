#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-11-13
# What: bootstrap
#----------------------------------------------------------------

library(here)
library(tidyverse)
#install.packages("boot")
library(boot)
library(kableExtra)

data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

# Compute the 95% bootstrap confidence intervals for the differences in medians of
# Infant mortality
# Under-5 mortality
# Neonatal mortality
# by armed conflict.
# Interpret the your findings.

#-----------------------------------------------------------------
# Maternal Mortality
#-----------------------------------------------------------------

data2017 <- data %>%
  filter(year == 2017) %>%
  drop_na(matmor)

data2017 |>
  group_by(armconflict) |>
  summarise(n = n(),
            median.matmor = median(matmor, na.rm = T))

matmor.arm1 <- data |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconflict == 1) |>
  dplyr::select(ISO, matmor)
matmor.arm0 <- data |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconflict == 0) |>
  dplyr::select(ISO, matmor)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$matmor) - median(resamp.arm0$matmor)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$matmor, sample_data$armconflict, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout.mat <- boot(data2017, statistic = getmeddiff, strata = data2017$armconflict, R = 1000)
bootout.mat

bootout.mat$t0
head(bootout.mat$t)
sd(bootout.mat$t)

boot.ci(boot.out = bootout.mat, conf = 0.95, type = c("bca"))


#-----------------------------------------------------------------
# Infant Mortality
#-----------------------------------------------------------------

data2017 <- data %>%
  filter(year == 2017) %>%
  drop_na(infantmor)

data2017 |>
  group_by(armconflict) |>
  summarise(n = n(),
            median.infantmor = median(infantmor, na.rm = T))

infantmor.arm1 <- data |>
  dplyr::filter(year == 2017 & !is.na(infantmor) & armconflict == 1) |>
  dplyr::select(ISO, infantmor)
infantmor.arm0 <- data |>
  dplyr::filter(year == 2017 & !is.na(infantmor) & armconflict == 0) |>
  dplyr::select(ISO, infantmor)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- infantmor.arm1[sample(nrow(infantmor.arm1), size = nrow(infantmor.arm1), replace = TRUE),]
  resamp.arm0 <- infantmor.arm0[sample(nrow(infantmor.arm0), size = nrow(infantmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$infantmor) - median(resamp.arm0$infantmor)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$infantmor, sample_data$armconflict, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout.infant <- boot(data2017, statistic = getmeddiff, strata = data2017$armconflict, R = 1000)
bootout.infant

bootout.infant$t0
head(bootout.infant$t)
sd(bootout.infant$t)

boot.ci(boot.out = bootout.infant, conf = 0.95, type = c("bca"))

#-----------------------------------------------------------------
# Neonatal Mortality
#-----------------------------------------------------------------

data2017 <- data %>%
  filter(year == 2017) %>%
  drop_na(neonatmor)

data2017 |>
  group_by(armconflict) |>
  summarise(n = n(),
            median.neonatmor = median(neonatmor, na.rm = T))

neonatmor.arm1 <- data |>
  dplyr::filter(year == 2017 & !is.na(neonatmor) & armconflict == 1) |>
  dplyr::select(ISO, neonatmor)
neonatmor.arm0 <- data |>
  dplyr::filter(year == 2017 & !is.na(neonatmor) & armconflict == 0) |>
  dplyr::select(ISO, neonatmor)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- neonatmor.arm1[sample(nrow(neonatmor.arm1), size = nrow(neonatmor.arm1), replace = TRUE),]
  resamp.arm0 <- neonatmor.arm0[sample(nrow(neonatmor.arm0), size = nrow(neonatmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$neonatmor) - median(resamp.arm0$neonatmor)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$neonatmor, sample_data$armconflict, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout.neonat <- boot(data2017, statistic = getmeddiff, strata = data2017$armconflict, R = 1000)
bootout.neonat

bootout.neonat$t0
head(bootout.neonat$t)
sd(bootout.neonat$t)

boot.ci(boot.out = bootout.neonat, conf = 0.95, type = c("bca"))

#-----------------------------------------------------------------
# Under 5 Mortality
#-----------------------------------------------------------------

data2017 <- data %>%
  filter(year == 2017) %>%
  drop_na(under5mor)

data2017 |>
  group_by(armconflict) |>
  summarise(n = n(),
            median.under5mor = median(under5mor, na.rm = T))

under5mor.arm1 <- data |>
  dplyr::filter(year == 2017 & !is.na(under5mor) & armconflict == 1) |>
  dplyr::select(ISO, under5mor)
under5mor.arm0 <- data |>
  dplyr::filter(year == 2017 & !is.na(under5mor) & armconflict == 0) |>
  dplyr::select(ISO, under5mor)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- under5mor.arm1[sample(nrow(under5mor.arm1), size = nrow(under5mor.arm1), replace = TRUE),]
  resamp.arm0 <- under5mor.arm0[sample(nrow(under5mor.arm0), size = nrow(under5mor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$under5mor) - median(resamp.arm0$under5mor)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$under5mor, sample_data$armconflict, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout.under5 <- boot(data2017, statistic = getmeddiff, strata = data2017$armconflict, R = 1000)
bootout.under5

bootout.under5$t0
head(bootout.under5$t)
sd(bootout.under5$t)

boot.ci(boot.out = bootout.under5, conf = 0.95, type = c("bca"))

#-----------------------------------------------------------------
# Create table 
#-----------------------------------------------------------------

mat_results <- data.frame(
  variable = "Maternal Mortality (N = 185)", 
  median_difference = bootout.mat$t0,
  ci_lower = boot.ci(boot.out = bootout.mat, cont = 0.95, type = c("bca"))$bca[4],
  ci_upper = boot.ci(boot.out = bootout.mat, cont = 0.95, type = c("bca"))$bca[5]
)

infant_results <- data.frame(
  variable = "Infant Mortality (N = 185)", 
  median_difference = bootout.infant$t0,
  ci_lower = boot.ci(boot.out = bootout.infant, cont = 0.95, type = c("bca"))$bca[4],
  ci_upper = boot.ci(boot.out = bootout.infant, cont = 0.95, type = c("bca"))$bca[5]
)

neonat_results <- data.frame(
  variable = "Neonatal Mortality (N = 185)", 
  median_difference = bootout.neonat$t0,
  ci_lower = boot.ci(boot.out = bootout.neonat, cont = 0.95, type = c("bca"))$bca[4],
  ci_upper = boot.ci(boot.out = bootout.neonat, cont = 0.95, type = c("bca"))$bca[5]
)

under5_results <- data.frame(
  variable = "Under 5 Mortality (N = 185)", 
  median_difference = bootout.under5$t0,
  ci_lower = boot.ci(boot.out = bootout.under5, cont = 0.95, type = c("bca"))$bca[4],
  ci_upper = boot.ci(boot.out = bootout.under5, cont = 0.95, type = c("bca"))$bca[5]
)

results <- bind_rows(mat_results, infant_results, neonat_results, under5_results)
print(results)

# The mean difference by armed conflict exposure is greatest for maternal mortality, followed by under 5 mortality, infant mortality, and neonatal mortality.
