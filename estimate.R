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
library(dplyr)
library(shinycssloaders)
library(doParallel)
library(parallel)
require(RcppRoll)

cl <- parallel::makeCluster(8)
registerDoParallel(cl)

## Reading all data
d_Austria_per_county <- read.csv("https://raw.githubusercontent.com/osaukh/dashcoch-AT/master/data_AT/covid19_cases_austria.csv", fileEncoding="UTF-8-BOM")
d_ages_R_eff <- read.csv("https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff.csv", sep=";", dec=",")
d_ages_States <- read.csv("https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff_bundesland.csv", sep=";", dec=",")

date_to_weekday <- Vectorize(function(input_date) {
  if (input_date %in% holiday_dates) {
    return("Holiday")
  } else if (wday(input_date) == 7 || wday(input_date) == 1) {
    return("Weekend")
  } else {
    return("Workday")
  }
})

calculate <- function(state, window_size) {
  library(EpiEstim)
  library(incidence)
  library(lubridate)
  library(utils)
  library(EnvStats)
  library(pracma)
  library(dplyr)
  require(RcppRoll)
  
  states_ages_map <- c(
    "W" = "Wien",
    "NÖ" = "Niederösterreich",
    "OÖ" = "Oberösterreich",
    "T" = "Tirol",
    "K" = "Kärnten",
    "ST" = "Steiermark",
    "S" = "Salzburg",
    "V" = "Vorarlberg",
    "B" = "Burgenland"
  )
  
  # CSH/Github pro Bundesland
  cases <- diff(d_Austria_per_county[,state])
  dates <- ymd(d_Austria_per_county$Date[-1])
  
  # Some states have negative cases, these will be applied to the previous day and set to 0
  repeat{
    found_negative_cases <- FALSE
    for (i in 2:length(cases)) {
      if(cases[i] < 0){
        print(sprintf("Warning negative cases found on day %s correcting by applying those corrections to the previous day", dates[i]))
        cases[i-1] <- cases[i-1] + cases[i]
        cases[i] <- 0
        found_negative_cases <- TRUE
      }
    }
    if (!found_negative_cases) { break }
  }
  
  min_cases_in_window <- 12 # implies a maximum CV of 0.3 of the posterior, see Corie et al. Web Appendix 2.	Choice of time window
  starting_window <- 15
  
  
  # Select only the data which contains enough cases in a window, starting with an offset 
  left_tail_matches <- cumany(roll_sum(cases, window_size, fill=min_cases_in_window, align="left") >= min_cases_in_window)
  right_tail_matches <- roll_sum(cases, window_size, fill=min_cases_in_window, align="right") >= min_cases_in_window
  right_tail_matches[1:starting_window] <- TRUE
  right_tail_matches <- cumall(right_tail_matches)
  filter_min_window_size <- left_tail_matches & right_tail_matches
  
  case_incidence <- as.incidence(cases[filter_min_window_size], dates = dates[filter_min_window_size])
  
  T <- case_incidence$timespan
  window_size_offset <- window_size - 1
  t_start <- seq(2, T-window_size_offset) # starting at 2 as conditional on the past observations
  t_end <- t_start + window_size_offset
  
  # obtained from: .Richter, L. et al. Schätzung des seriellen Intervalles von COVID19, Österreich. 3.
  si_mean = 4.46
  si_mean_std = 0.153 # calculated from 95% ci from the paper assuming normality by centering and divison by 1.96
  si_std_mean = 2.63
  si_std_std = 0.133 # calculated from 95% ci from the paper assuming normality as above
  
  
  config_uncertain <- make_config(list(mean_si = si_mean, std_mean_si =si_mean_std,
                                       min_mean_si = si_mean - si_mean_std * 1.96, max_mean_si = si_mean + si_mean_std * 1.96,
                                       std_si = si_std_mean, std_std_si = si_std_std,
                                       min_std_si = si_std_mean - si_std_std * 1.96, max_std_si = si_std_mean + si_std_std * 1.96,
                                       t_start=t_start, t_end=t_end,
                                       mean_prior = 2.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  config_parametric <- make_config(list(mean_si = si_mean, std_si = si_std_mean,
                                        t_start=t_start, t_end=t_end,
                                        mean_prior = 2.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  estimation_result <- estimate_R(case_incidence, 
                                  method="uncertain_si",
                                  config = config_uncertain)
  
  # Added state or country wide AGES data
  if(state == "AT") {
    d_ages <- d_ages_R_eff
  } else {
    d_ages <- d_ages_States[d_ages_States$Bundesland == states_ages_map[state],]
  }
  
  return(list(
    state = state,
    dates = dates,
    cases = cases,
    case_incidence = case_incidence,
    window_size = window_size,
    estimated_R = estimation_result$R,
    ages_estimate = d_ages
  ))
}

data_result = list()
states <- c("AT", "W", "NÖ", "OÖ", "ST", "T", "K", "S", "V", "B")


for (state in states) {
  print(sprintf("Starting calculation for state=%s", state))
  data_result[[state]] <- foreach(tau=3:20) %dopar% calculate(state, tau)
}

saveRDS(data_result, "r_estimate/data/data.rds")

stopCluster(cl)