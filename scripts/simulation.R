#-----------------------------------------------------------------
# Author: Amrit Tiwana
# Last updated: 2023-11-06
# What: simulation study on lung cancer (y), coffee (x), smoking (z)
#----------------------------------------------------------------

# Simulation study

library (SimDesign)

Design <- createDesign(sample_size = c (50 , 500),
                     pz = c (0.2, 0.8),
                     alpha0 = c (-1, 0, 1),
                     alpha1 = c (0, 0.5, 1))

Generate <- function(condition, fixed_objects = NULL) {
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  pz <- condition$pz
  n <- condition$sample_size
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  dat
}

Analyse <- function (condition, dat, fixed_objects = NULL) {
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  ret <- c(unadj.coef[2,4], adj.coef[2,4])
  ret
}

Summarise <- function(condition, results, fixed_objects = NULL) {
  ret <- EDR(results, alpha = 0.05)
  ret
}

results <- runSimulation (design = Design, replications = 1000,
                           generate = Generate, analyse = Analyse, summarise = Summarise)

# Queston 2

library(tidyverse)
library(ggplot2)

resultslong <- results %>%
  pivot_longer(cols = c("V1", "V2"),
                   names_to = "model",
                   values_to = "edr")

resultslong$sample_size <- factor(resultslong$sample_size, levels = c(50,100),
                                  ordered = T, labels=c("n==50", "n==100"))



themin <- min(resultslong$alpha1)
themax <- max(resultslong$alpha1)

p <- ggplot(resultslong, aes(x=alpha1, y=edr, fill=model)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype = "longdash") +
  labs(y = "Relative bias", x =  bquote(alpha[1]~values)) + 
  theme_bw() +
  facet_wrap(~ varlab, scales = "free", labeller = label_parsed) +
  scale_fill_grey(start = 1, end = 0.4, 
                  name="Method",
                  breaks=c("ccbeta3relbias", "cwbeta3relbias", "bbdcwbeta3relbias", "rrzcwbeta3relbias"),
                  labels=c("CCGEE", "CWGEE", "DWGEE", "IPC-CWGEE")) +
  scale_y_continuous(limits = c(themin, themax)) 

# unadjusted model overestimates the association as alpha1 increases
p