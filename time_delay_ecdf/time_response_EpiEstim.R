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


beta_t <- function(t, parameters) {
  with(as.list(c(parameters)),{
    logistic <- 1/(1+exp(-logistic_k*(t-t_swap)))
    beta <- (1-logistic) * beta0 + logistic*beta1
    
    return(beta)
  })
}

R0_t <- function(t, parameters) {
  with(as.list(c(parameters)),{
    logistic <- 1/(1+exp(-logistic_k*(t-t_swap)))
    beta <- (1-logistic) * beta0 + logistic*beta1
    return((beta * sigma)/(gamma*sigma))
  })
}



gamma_cdf_mean_sigma <- function(k, mu, sigma) 
{
  if (sigma < 0) {
    stop("sigma must be >=0.")
  }
  if (mu <= 1) {
    stop("mu must be >1")
  }
  if (any(k < 0)) {
    stop("all values in k must be >=0.")
  }
  
  a <- ((mu - 1) / sigma)^2
  b <- sigma^2 / (mu - 1)
  
  return(pgamma(k, shape = a, scale = b))
}

SEIR <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    logistic <- 1/(1+exp(-logistic_k*(t-t_swap)))
    beta <- (1-logistic) * beta0 + logistic*beta1
    lambda <- beta*I/N
    dS <- -lambda*S
    dE <- lambda*S - sigma*E
    dI <- sigma*E - gamma*I
    dR <- gamma*I
    list(c(dS, dE, dI, dR))
  })
}


N = 5*10**7
E0 = 1
S0 = N - E0
parameters <- c(
  logistic_k = 20,
  gamma = 1/3,
  sigma = 1/4,
  beta0 = 0.6,
  beta1 = 0.8,
  t_swap = 70,
  N = N
)

state <- c(
  S = S0,
  E = E0,
  I = 0,
  R = 0
)

times <- seq(1,400, by=1)

SEIR_solved <- ode(y = state, times = times, func = SEIR, parms = parameters)


t0 <- 30
tn <- 100
cases <- round(-1*diff(SEIR_solved[,"S"]))[t0:tn]
start_date <- ymd("2050-01-01")
dates <- start_date + days(seq(1, length(cases)))

case_incidence <- as.incidence(round(cases), dates = dates)

# obtained from: .Richter, L. et al. Schätzung des seriellen Intervalles von COVID19, Österreich. 3.
si_mean = 4.46
si_mean_std = 0.153 # calculated from 95% ci from the paper assuming normality by centering and divison by 1.96
si_std_mean = 2.63
si_std_std = 0.133 # calculated from 95% ci from the paper assuming normality as above


for (window_size in seq(3,20)) {
  T <- length(cases)
  window_size_offset <- window_size - 1
  t_start <- seq(2, T-window_size_offset) # starting at 2 as conditional on the past observations
  t_end <- t_start + window_size_offset
  
  
  config_parametric <- make_config(list(mean_si = si_mean, std_si = si_std_mean,
                                        t_start=t_start, t_end=t_end,
                                        mean_prior = 2.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  res_parametric_si <- estimate_R(case_incidence, 
                                  method="parametric_si",
                                  config = config_parametric)
  
  # Store step response function
  r_step_response <- res_parametric_si$R$`Mean(R)`[(parameters["t_swap"]-t0-window_size+1):(parameters["t_swap"]-t0)]
  r_step_response <- r_step_response - res_parametric_si$R$`Mean(R)`[(parameters["t_swap"]-t0-window_size)]
  r_step_response <- r_step_response/tail(r_step_response, 1)
  saveRDS(r_step_response, sprintf("time_delay_ecdf/out/time_response_tau_%d.rds", window_size))
  
  # Plot time response for this window size
  dates <- seq(1, length(cases)) 
  r_plot <- ggplot(res_parametric_si$R, aes(x=dates[t_end]))
  
  r_plot <- r_plot +  geom_line(aes(y=`Median(R)`), color="#264653", size=0.9, alpha=0.7) +
    geom_point(aes(y=`Median(R)`), alpha=0.7, size=2, color="#264653") + 
    geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.7) +
    geom_vline(xintercept = parameters["t_swap"] - t0, color="#d66449", alpha=0.7, size=1) +
    geom_vline(xintercept = parameters["t_swap"] - t0+ (window_size), color="#d66449", alpha=0.7, size=1) +
    annotate("text",x=parameters["t_swap"] - t0 + window_size/2,y=1.4,label=TeX("$\\tau$"), size=7) + 
    xlab("t") +
    ylab(TeX("$R_{t,\\tau}$")) +
    coord_cartesian(ylim = c(1.3, 2)) +
    grid() +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(30,70)
  
  ggsave(sprintf("time_delay_ecdf/out/plots/time_response_tau_%d.pdf", window_size), plot=r_plot, units = "cm", width =22, height=6, dpi="print")
}


cases_with_dates <- data.frame("Date" = dates[-(1:window_size)], "Cases" = cases[-(1:window_size)])
cases_plot <- ggbarplot(cases_with_dates, x="Date", y="Cases", fill="#e19257", alpha=0.6, color = NA) + 
  labs(fill = "") +
  xlab("t") +
  xlim(30,70) 


beta_t_support <- seq(t0,tn,0.1)
beta_t_plot <- data.frame("Date" = beta_t_support - t0, "beta" = beta_t(beta_t_support, parameters))
beta_plot <- ggplot(data=beta_t_plot, aes(x=Date)) + 
  geom_line(aes(y=beta)) +
  geom_vline(xintercept = parameters["t_swap"] - t0, color="#d66449", alpha=0.4, size=1) +
  theme_pubr() +
  xlim(30,70) +
  xlab("t") +
  ylab(TeX("$\\beta(t)$"))+
  grid()

ggsave("time_delay_ecdf/out/plots/time_response_cases.pdf", plot=cases_plot, units = "cm", width =22, height=6, dpi="print")
ggsave("time_delay_ecdf/out/plots/time_response_beta.pdf", plot=beta_plot, units = "cm", width =22, height=4, dpi="print")

