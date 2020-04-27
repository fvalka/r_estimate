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

## Simulate time delay distributions
simSize <- 1e6
sim_onset_reporting <- rgamma(simSize, 1.88, 0.26) # onset-to-reporting distribution
sim_incubtion <- rgamma(simSize, shape = 4.23, rate = 0.81) # incubation period

ecdf_incubation_reporting <- ecdf(sim_onset_reporting + sim_incubtion)

## Reading all data
d_Austria_per_county <- read.csv("https://raw.githubusercontent.com/osaukh/dashcoch-AT/master/data_AT/covid19_cases_austria.csv", fileEncoding="UTF-8-BOM")
d_ages_R_eff <- read.csv("https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff.csv", sep=";", dec=",")
d_holidays <- read.csv("data/nager-date/publicholiday.AT.2020.csv", fileEncoding="UTF-8-BOM")
holiday_dates <- ymd(d_holidays$Date)

date_to_weekday <- Vectorize(function(input_date) {
  if (input_date %in% holiday_dates) {
    return("Holiday")
  } else if (wday(input_date) == 7 || wday(input_date) == 1) {
    return("Weekend")
  } else {
    return("Workday")
  }
})


plotRcombined <- function(state, window_size, lockdown_date, plot_ages) {
  # CSH/Github pro Bundesland
  cases <- diff(d_Austria_per_county[,state])
  dates <- ymd(d_Austria_per_county$Date[-1])
  last_date <- tail(dates, 1)
  dates_all_plot <- last_date - days(45) + days(seq(0:44))
  
  # Some states have negative cases, these will be applied to the previous day and set to 0
  for (i in 2:length(cases)) {
    if(cases[i] < 0){
      print(sprintf("Warning negative cases found on day %s correcting by applying those corrections to the previous day", dates[i]))
      cases[i-1] <- cases[i-1] + cases[i]
      cases[i] <- 0
    }
  }
  
  case_incidence <- as.incidence(cases, dates = dates)
  
  T <- case_incidence$timespan
  #window_size <- 7
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
                                       mean_prior = 6.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  config_parametric <- make_config(list(mean_si = si_mean, std_si = si_std_mean,
                                        t_start=t_start, t_end=t_end,
                                        mean_prior = 2.59 # obtained from: Liu, Q. et al. Assessing the Global Tendency of COVID-19 Outbreak.
  ))
  
  res_parametric_si <- estimate_R(case_incidence, 
                                  method="uncertain_si",
                                  config = config_uncertain)
  
  head(res_parametric_si$R)
  
  R_with_dates <- data.frame("Date" = case_incidence$dates[-(1:window_size)], "R_median" = res_parametric_si$R$`Median(R)`)
  cases_with_dates <- data.frame("Date" = dates, "Cases" = cases, 
                                 "Weekday" = date_to_weekday(dates))
  
  r_plot <- ggplot(data = res_parametric_si$R, aes(x=dates[t_end]))

  # Add half a day to obtain the midpoint between t_Start and t_End
  delay_ecdf <- movavg(ecdf_incubation_reporting(difftime(dates_all_plot, lockdown_date , units = c("days")) + 0.5), n=window_size, type = "s")
  delay_ecdf_plot_data <- data.frame("Start" = dates_all_plot, "End" = dates_all_plot + days(1), "Delay CDF" = delay_ecdf)
  
  # Workaround: Setting x causes a warning, but not setting x causes an error
  r_plot <- r_plot + geom_rect(data = delay_ecdf_plot_data, aes(x=Start, xmin=Start, xmax=End, ymin=0, ymax=10, fill=Delay.CDF), color=NA, alpha=0.5) +
    scale_fill_gradient2(low="white", mid="#f4e085", high="#91ebe9", midpoint=0.5, limits=c(0,1))
  
  colors <- c("Own" = "#264653", "AGES" = "#e76f51")
  
  r_plot <- r_plot +  geom_line(aes(y=`Median(R)`, color="Own")) +
    geom_point(aes(y=`Median(R)`, color="Own"), alpha=0.5) + 
    geom_ribbon(aes(ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`), alpha=0.5, fill="#5f7e87") + 
    geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5) +
    geom_vline(xintercept = lockdown_date, color="#d66449", alpha=0.3, size=2) +
    annotate(geom = "text", x = lockdown_date, y = 0.1, label="Intervention", color="#822c20", hjust = 0, vjust=-0.4, angle = 90) +
    ggtitle(TeX("Sliding time window $R_{t,\\tau}$ Estimation")) +
    xlab("Date") +
    ylab(TeX(paste("$R_{t,\\tau}$ with sliding time window $\\,\\tau =", window_size, "$ days"))) +
    coord_cartesian(ylim = c(0, 3.5), xlim=c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
    grid() +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))+
    labs(color = "Source") +
    scale_color_manual(values = colors)
  
  if(plot_ages) {
    r_plot <- r_plot + geom_line(data = d_ages_R_eff, aes(x=ymd(Datum), y=R_eff, color="AGES"), alpha=0.7) +
      geom_ribbon(data = d_ages_R_eff, mapping =  aes(x=ymd(Datum), ymin=R_eff_lwr, ymax=R_eff_upr), alpha=0.5, fill="#e76f51")
  }
  
  colors_weekday <- c("Workday" = "#e19257", "Weekend" = "#ad663d", "Holiday" = "#450f09")
  cases_plot <- ggbarplot(cases_with_dates, x="Date", y="Cases", fill="Weekday", alpha=0.6, color = NA) + 
    labs(fill = "") +
    ylab("New cases per day") + 
    coord_cartesian(xlim = c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
    scale_fill_manual(values = colors_weekday)
  
  return(ggarrange(r_plot, cases_plot, ncol = 1, nrow = 2, heights = c(2, 0.9), align = "v"))
}

ui <- fluidPage(
  withMathJax(),
  titlePanel("Estimation of the time dependent reproduction number R based on reported cases in Austria"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", h3("State"), 
                  choices = list("Whole country" = "AT", 
                                 "Vienna" = "W",
                                 "Lower Austria" = "NÖ",
                                 "Upper Austria" = "OÖ",
                                 "Styria" = "ST",
                                 "Tyrol" = "T",
                                 "Carinthia" = "K",
                                 "Salzburg" = "S",
                                 "Vorarlberg" = "V",
                                 "Burgenland" = "B",
                                 "Tirol" = "T"), 
                  selected = "AT"),
      dateInput("intervention_date", "Intervention date", ymd("2020-03-16")),
      sliderInput(inputId = "tau", label = "Sliding time window \\(\\tau\\) [days]", 
                  min = 3,
                  max = 20,
                  value = 7),
      checkboxInput("plot_ages", "Plot AGES \\(\\tau = 13\\) days \\(R_{t,\\tau}\\) estimate for Austria", FALSE)
    ),
    mainPanel(
      plotOutput(outputId = "combinedRplot", height="70vh") %>% withSpinner(color="#4c666f"),
      h3("Delay.CDF"),
      helpText("The background color gradient shows the Delay.CDF which is an estimate of the cumulative distribution 
                function of three different time delays introduced into the estimation of \\(R_{t,\\tau}\\). 
                These time delays are the time from infection to symptom onset (incubation time) \\(t_{infection,onset}\\), the time
                from onset to reporting \\(t_{onset,reporting}\\) and the time delay introduced by the \\(R_{t,\\tau}\\) methodology 
                of assuming a fixed \\(R_t\\) within the time window \\(\\tau\\) which we will call \\(t_{reporting,estimation}\\)."),
      
      helpText("Estimates for the combined time delay of both \\(t_{infection,onset}\\) and \\(t_{onset,reporting}\\) are obtained 
              through Monte Carlo addition. This means drawing a large sample of random values \\(t_{infection,onset}^i\\) and 
              \\(t_{onset,reporting}^i\\) from the 
              \\(\\Gamma\\) distributions published for the estimates of those time delays and then
              adding the drawn samples together \\(t_{infection,reporting}^i = t_{infection,onset}^i + t_{onset,reporting}^i \\).
              The empirical cumulative distribution function 
              \\(\\displaystyle{\\widehat {F}}_{infection,reporting}(t)\\) of the combined samples is then obtained."),
      
      helpText("Published parameter estimates for the \\(\\Gamma\\) distributions used were originally obtained from Chinese case data. 
               This assumption has possible implications for the applicability of the estimation in the current context, since the reporting time 
               \\(t_{onset,reporting}\\) may vary significantly, depending on the circumstances."),
      
      helpText("Based upon an intervention date \\(t_{intervention}\\) the ECDF is used to obtain the fraction of cases infected on the 
                day of the intervention and expected to have been reported by the day \\(t_j\\) as follows:
               
               $$ \\displaystyle{\\widehat {F}}_{infection,reporting}\\left(t_j - t_{intervention} + \\frac{1}{2}\\right) $$"),
      
      helpText("In the final step the time delay from reporting to esimtation \\(t_{reporting,estimation}\\) is considered.
                To approximate the effect of the assumption of \\(R_t\\) being constant within the sliding time window \\(\\tau\\) a 
                simple moving average with a backward window length of \\(\\tau\\) is applied to 
               \\(\\displaystyle{\\widehat {F}}_{infection,reporting}(t_j - t_{intervention})\\).
               "),
      h3("Data sources"),
      h4("Case data for Austria"),
      tags$div(checked=NA,
               tags$a(href="https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html", 
                      "Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz"),
               ". Aggregated by the ",
               tags$a(href="https://www.csh.ac.at/", " Complexity Science Hub Vienna"),
               " and published on ",
               tags$a(href="https://github.com/osaukh/dashcoch-AT", "GitHub"),
               ".",
               tags$br(),
               "The data as published at the 15:00 CET deadline is used in this dataset."
      ),
      h4("AGES \\(R_{t,\\tau}\\) estimate"),
      tags$div(checked=NA,
               tags$a(href="https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/", 
                      "AGES - Österreichische Agentur für Gesundheit und Ernährungssicherheit GmbH")
      )
    )
  )
)

server <- function(input, output) {
  output$combinedRplot <- renderPlot({
    plotRcombined(input$county, input$tau, input$intervention_date, input$plot_ages)
  })
}

shinyApp(ui = ui, server = server)