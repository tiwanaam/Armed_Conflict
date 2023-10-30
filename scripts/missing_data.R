#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-30
# What: impute missing data
#----------------------------------------------------------------

# Read in the final data set

library(here)
finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

# Load package

library(naniar)
library(VIM)
library(finalfit)
library(texreg)
library(mice)
library(tidyverse)

# Use one of the missing data visulization pacakges to describe the patterns of missing data 

naniar::vis_miss(finaldata)
VIM::aggr(finaldata, numbers = TRUE, prop = c(TRUE, FALSE))

# Create linear models for each outcome

finaldata$popdens100 <- finaldata$popdens / 100

finaldata$GDP1000 <- finaldata$GDP / 1000

preds <- as.formula(" ~ armconflict + earthquake + temp + drought + male_edu + agedep + urban + popdens100 + GDP1000 + OECD + ISO + as.factor(year)")

matmormod <- lm(update.formula(preds, matmor ~ .), data = finaldata)
infantmormod <- lm(update.formula(preds, infantmor ~ .), data = finaldata)
neonatmormod <- lm(update.formula(preds, neonatmor ~ .), data = finaldata)
under5mormod <- lm(update.formula(preds, under5mor ~ .), data = finaldata)

keepvars <- list("armconflict" = "Armed Conflict",
                 "GDP1000 = GDP",
                 "podens100" = "Population Density",
                 "urban" = "Urban", 
                 "agedep" = "Age Dependency",
                 "male_edu" = "Male Education",
                 "temp" = "Average Temperature",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")

screenreg(list(matmormod, infantmormod, neonatmormod, under5mormod),
       ci.force = TRUE,
       custom.coef.map = keepvars,
       custom.model.names = c("Maternal mortality", "Under 5 Mortality", "Infant Mortality", "Neonatal Mortality"),
       caption="Results from linear regression models")

# Use the mice package to multiply impute the final data with 𝑚 = 10 imputations

finaldata <- finaldata %>%
  select(-c("country_name", "region", "ISO", "GDP", "popdens"))

finaldata$ISOfac <- as.numeric(factor(finaldata$ISO))

meth[c("GDP", "popdens", "urban", "male_educ", "temp", "matmor", "infantmor", "neonatmor", "under5mor", "popdens100", "GDP1000")] <- "2l.bin"

set.seed(100) 

start.time <- Sys.time()

mice.out <- mice(finaldata, seed = 1, m = 10, method=c("","", "", "pmm", "", "pmm","pmm", "pmm", "pmm", "pmm", "pmm", "", "", "", "", "pmm", "pmm", ""), 
                 maxit = 20, 
                 print = F)

mice.out$method
class(finaldata$GDP1000)

end.time <- Sys.time()

end.time - start.time

mi0 <- mice(finaldata, seed = 1, m = 1, maxit = 0, print = F)
mi0$method

# Plot the mice 
plot(mice.out)

# Extract the complete data
complete.data <- complete(mice.out, "all")

# Check the first imputed data set
head(complete.data$`1`)

fit.mice <- with(mice.out, lm(matmor ~ armconflict + earthquake + temp + drought + male_edu + agedep + urban + popdens100 + GDP1000 + OECD + ISOfac + as.factor(year)))

# the pool() function from mice will calculate the pooled result
out.mice <- pool(fit.mice)
summary(out.mice)
