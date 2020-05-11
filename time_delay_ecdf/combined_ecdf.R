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
require(Cairo)
require(RcppRoll)

ecdf_incubation_reporting_precalculated <- readRDS("r_estimate/data/time-delay/infection_reporting_cdf.rds")
ecdf_incubation_reporting <- Vectorize(function(t) {
  idx <- findInterval(t, ecdf_incubation_reporting_precalculated$t, all.inside=TRUE)
  return(ecdf_incubation_reporting_precalculated$infection_reporting_cdf[idx])
})

combined_ecdf <- function(window_size) {
  estimation_delay <- readRDS(paste0("time_delay_ecdf/out/time_response_tau_",window_size,".rds"))
  
  result <- rep(0, 41)
  
  for(i in 0:40) {
    result[i+1] <- sum(diff(append(c(0), estimation_delay))*ecdf_incubation_reporting(seq(i + 0.5,i + 0.5 - window_size+1)))
  }
  
  return(result)
}


combined_ecdfs <- data.frame("t"=seq(0,40))

for (tau in seq(3,20)) {
  combined_ecdfs[[sprintf("tau_%d", tau)]] <- combined_ecdf(tau)
}

saveRDS(combined_ecdfs, "r_estimate/data/time-delay/infection_estimation_cdf.rds")
