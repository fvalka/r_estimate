library(dplyr)
library(ggplot2)
library(ggpubr)

simSize <- 100000
sim_latent <- rgamma(simSize, shape=2.88, scale = 1.55)
sim_incubation <- rgamma(simSize, shape = 3.18, rate = 0.59)

sim_infectious_around_symptoms <- ecdf(sim_incubation - sim_latent)

x<-seq(-20,20,0.1)
sim_infectious_around_symptoms_plot <- data.frame(
  x=x,
  ecdf=sim_infectious_around_symptoms(x)
)

sim_infectious_around_symptoms_plot %>%
  ggplot(aes(x=x)) +
  geom_line(aes(y=ecdf, color="Difference between serial interval and incubation time interval")) +
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = 14) +
  scale_x_continuous(breaks = c(-20,0,7, 14, 20)) +
  labs(color="",
       caption = "Sources:
        Incubation interval: 
        Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
       a descriptive and modelling study. The Lancet Infectious Diseases, (2020),
       
       Serial interval for Austria: 
       Richter, L. et al. Schätzung des seriellen Intervalles von COVID19, Österreich.") +
  ylab("CDF") + 
  theme_pubclean()
