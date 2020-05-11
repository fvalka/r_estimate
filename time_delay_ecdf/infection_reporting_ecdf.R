library(EpiEstim)
library(ggplot2)
library(incidence)
library(lubridate)
library(utils)
library("ggpubr")
library(latex2exp)
library(foreach)
library(RColorBrewer)
library(shiny)
library(EnvStats)
library(pracma)
library(deSolve)
require(scales)

## Simulate time delay distributions
print("Computing ECDF")
simSize <- 1e7

# Obtained from the supplementary appendix of 
# .Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
# a descriptive and modelling study. The Lancet Infectious Diseases 0, (2020).
# Second time period Jan 28 – Feb 17
sim_onset_reporting <- rgamma(simSize, shape = 3.18, rate = 0.59) # onset-to-reporting distribution

# Obtained from the supplementary appendix of 
# .Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
# a descriptive and modelling study. The Lancet Infectious Diseases 0, (2020).
sim_incubtion <- rgamma(simSize, shape = 4.23, rate = 0.81) # incubation period

ecdf_infection_reporting <- ecdf(sim_onset_reporting + sim_incubtion)

t <- seq(0,40,0.1)
ecdf_incubation_reporting_discr <- data.frame("t"=t, "infection_reporting_cdf"=ecdf_infection_reporting(t))
saveRDS(ecdf_incubation_reporting_discr, "r_estimate/data/time-delay/infection_reporting_cdf.rds")
