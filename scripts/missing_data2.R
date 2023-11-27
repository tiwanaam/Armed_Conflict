#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-30
# What: impute missing data
#----------------------------------------------------------------

# Read in the final data set

library(here)
finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

# Missing data visualization 

library(naniar)
library(VIM)
library(finalfit)
library(knitr)

naniar::vis_miss(finaldata)
VIM::aggr(finaldata, numbers = TRUE, prop = c(TRUE, FALSE), plot=FALSE)

a <- aggr(finaldata)
a
summary(a)

explanatory = c("GDP","popdens","urban","male_edu","temp","matmor",
                "infantmor","neonatmor","under5mor")
dependent = "armconflict"

finaldata %>%
  ff_glimpse(dependent, explanatory)

finaldata %>%
  missing_pattern(dependent, explanatory)

finaldata %>%
  missing_compare(dependent, explanatory) # not working

finaldata %>% 
  finalfit::missing_compare(dependent="armconflict", 
                            explanatory=c("GDP","popdens","urban","male_edu","temp","matmor",
                                          "infantmor","neonatmor","under5mor")) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"),
               caption = "Comparison by armed conflict not missing vs not missing") %>% 
  kable_styling(full_width = F)

# Running the linear models 

# we convert variables from numeric type to factor type before running the regression

finaldata[, c("year")] <- 
  lapply(finaldata[, c("year")], factor)

# scale up GDP by 1,000 and rescale popdens from 0 to 1

finaldata$GDP1000 <- finaldata$GDP / 1000
finaldata$popdens100 <- finaldata$popdens / 100

preds <- as.formula("~ armconflict + earthquake + temp + drought + male_edu + agedep + urban + popdens100 + GDP1000 + OECD + ISO + year")

matmormod <- lm(update.formula(preds, matmor ~ .), data = finaldata)
infantmormod <- lm(update.formula(preds, infantmor ~ .), data = finaldata)
neonatmormod <- lm(update.formula(preds, neonatmor ~ .), data = finaldata)
under5mormod <- lm(update.formula(preds, under5mor ~ .), data = finaldata)

# Mice package 

library(mice)

finaldata2 <- finaldata %>%
  select(-c("country_name", "region", "ISO", "GDP", "popdens"))

set.seed(100) # set seed for reproducibility

# dry run to get the method vector
mi0 <- mice(finaldata2, seed = 1, m = 1, maxit = 0, print = F)
meth <- mi0$method
meth

# norm for continuous data, logreg for binary data
start.time <- Sys.time()
mice.out <- mice(finaldata2, seed = 1, m = 10, method=c("", "", "", "pmm", "" ,"pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "", "", "", "", "pmm", "pmm"), 
                 maxit = 20, print = FALSE)
end.time <- Sys.time()
end.time - start.time

# time difference of 25.08996 secs
plot(mice.out)

# extract complete data from mice output
complete.data <- complete(mice.out, "all")

# check the first imputed dataset
head(complete.data$`1`)

fit.mice <- with(mice.out, lm(matmor ~ armconflict + earthquake + temp + drought + male_edu + agedep + urban + popdens100 + GDP1000 + OECD + year))

# the pool() function from mice will calculate the pooled result
out.mice <- pool(fit.mice)
summary(out.mice)

library(howManyImputations)

# using pilot MI analysis above from mice
how_many_imputations(fit.mice)
