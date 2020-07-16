library(ggplot2)
library(ggpubr)
library(dplyr)


x<-seq(1,20,0.1)
covid_time_dist <- data.frame("x"=x, 
           "si_interval"=dgamma(x, shape=2.88, scale = 1.55),
           "incubation_time"=dgamma(x, shape = 3.18, rate = 0.59))

median_si <- median(covid_time_dist$si_interval)
median_incubation <- median(covid_time_dist$incubation_time)

covid_time_dist %>%
  ggplot(aes(x=x)) +
  geom_area(aes(y=si_interval, fill="Serial interval"), alpha=0.3) +
  geom_area(aes(y=incubation_time, fill="Incubation time"), alpha=0.3) +
  geom_line(aes(y=si_interval, color="Serial interval"), alpha=0.7) +
  geom_line(aes(y=incubation_time, color="Incubation time"), alpha=0.7) +
  #geom_vline(aes(xintercept = median_si, color="Seriell interval")) +
  theme_pubclean() +
  labs(caption = "Sources:
        Incubation interval: 
        Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
       a descriptive and modelling study. The Lancet Infectious Diseases, (2020),
       
       Serial interval for Austria: 
       Richter, L. et al. Schätzung des seriellen Intervalles von COVID19, Österreich.") +
  xlab("Days after infection") +
  ylab("Probability Density")
