#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-10-16
# What: Create Table 1
#----------------------------------------------------------------

data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

library(table1)

newdata <- subset(data, year==2000)

newdata$armconflict <- 
  factor(newdata$armconflict,
         levels=c(1,0),
         labels=c("Yes", "No"))

newdata$OECD <- 
  factor(newdata$OECD,
         levels=c(1,0),
         labels=c("Yes", "No"))

newdata$drought <- 
  factor(newdata$drought,
         levels=c(1,0),
         labels=c("Yes", "No"))

newdata$earthquake <- 
  factor(newdata$earthquake,
         levels=c(1,0),
         labels=c("Yes", "No"))

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 matmor = "Median [Min, Max]",
                 infantmor = "Median [Min, Max]",
                 neonatmor = "Median [Min, Max]",
                 under5mor = "Median [Min, Max]",
                 GDP = "Median [Min, Max]",
                 popdens = "Median [Min, Max]",
                 urban = "Median [Min, Max]",
                 agedep = "Median [Min, Max]",
                 male_edu = "Median [Min, Max]",
                 temp = "Median [Min, Max]")
  parse.abbrev.render.code(c("", what))(x)
}

table1(~ matmor + infantmor + neonatmor + under5mor + GDP + OECD + popdens + urban + agedep + male_edu + temp + drought + earthquake | armconflict, data=newdata, render=rndr)
