require(ggplot2)
require(lubridate)
require(utils)
require(ggpubr)
require(latex2exp)
require(shiny)
require(pracma)
require(shinycssloaders)

server <- function(input, output) {
  
  ## Simulate time delay distributions
  print("Computing ECDF")
  simSize <- 1e6
  
  # Obtained from the supplementary appendix of 
  # .Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
  # a descriptive and modelling study. The Lancet Infectious Diseases 0, (2020).
  # Second time period Jan 28 – Feb 17
  sim_onset_reporting <- rgamma(simSize, shape = 3.18, rate = 0.59) # onset-to-reporting distribution
  
  # Obtained from the supplementary appendix of 
  # .Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
  # a descriptive and modelling study. The Lancet Infectious Diseases 0, (2020).
  sim_incubtion <- rgamma(simSize, shape = 4.23, rate = 0.81) # incubation period
  
  ecdf_incubation_reporting <- ecdf(sim_onset_reporting + sim_incubtion)
  
  ## Reading all data
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
  
  plotRcombined <- function(data, lockdown_date, plot_ages) {
    
    first_date <- head(data$dates, 1)
    last_date <- tail(data$dates, 1)
    
    first_plot_date <- first_date + days(7)
    
    dates_all_plot <- first_plot_date + days(0:(last_date-first_plot_date))
    dates_all_plot_extended <- first_date + days(0:((last_date-first_plot_date)+10))
    
    R_with_dates <- data.frame("Date" = data$case_incidence$dates[-(1:data$window_size)], "R_median" = data$estimated_R$`Median(R)`)
    cases_with_dates <- data.frame("Date" = data$dates, "Cases" = data$cases, 
                                   "Weekday" = date_to_weekday(data$dates))
    
    r_plot <- ggplot(data = data$estimated_R, aes(x=data$case_incidence$dates[t_end]))
    
    # Add half a day to obtain the midpoint between t_Start and t_End
    delay_ecdf <- movavg(ecdf_incubation_reporting(difftime(dates_all_plot_extended, lockdown_date , units = c("days")) + 0.5), 
                         n=data$window_size, type = "s")
    delay_ecdf_plot_data <- data.frame("Start" = dates_all_plot_extended, "End" = dates_all_plot_extended + days(1), 
                                       "Delay CDF" = delay_ecdf)
    
    # Workaround: Setting x causes a warning, but not setting x causes an error
    r_plot <- r_plot + geom_rect(data = delay_ecdf_plot_data, aes(x=Start, xmin=Start, xmax=End, ymin=0, ymax=10, fill=Delay.CDF), color=NA, alpha=0.5) +
      scale_fill_gradient2(low="white", mid="#f4e085", high="#91ebe9", midpoint=0.5, limits=c(0,1))
    
    colors <- c("Own" = "#264653", "AGES" = "#e76f51")
    
    r_plot <- r_plot +  geom_line(aes(y=`Median(R)`, color="Own")) +
      geom_point(aes(y=`Median(R)`, color="Own"), alpha=0.5) + 
      geom_ribbon(aes(ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`), alpha=0.5, fill="#5f7e87") + 
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5) +
      geom_vline(xintercept = lockdown_date, color="#d66449", alpha=0.3, size=2) +
      annotate(geom = "text", x = lockdown_date, y = 0.1, label="Infection", color="#822c20", hjust = 0, vjust=-0.4, angle = 90) +
      #ggtitle(TeX("Sliding time window $R_{t,\\tau}$ Estimation")) +
      xlab("Date") +
      ylab(TeX(paste("$R_{t,\\tau}$ with sliding time window $\\,\\tau =", data$window_size, "$ days"))) +
      coord_cartesian(ylim = c(0, 3.5), xlim=c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
      grid() +
      theme_pubr() +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(color = "Source", fill=TeX("CDF time infection $\\rightarrow$ estimation")) +
      scale_color_manual(values = colors)
    
    if(plot_ages) {
      r_plot <- r_plot + geom_line(data = data$ages_estimate, aes(x=ymd(Datum), y=R_eff, color="AGES"), alpha=0.7) +
        geom_ribbon(data = data$ages_estimate, mapping =  aes(x=ymd(Datum), ymin=R_eff_lwr, ymax=R_eff_upr), alpha=0.5, fill="#e76f51")
    }
    
    colors_weekday <- c("Workday" = "#e19257", "Weekend" = "#ad663d", "Holiday" = "#450f09")
    cases_plot <- ggbarplot(cases_with_dates, x="Date", y="Cases", fill="Weekday", alpha=0.6, color = NA) + 
      labs(fill = "") +
      ylab("New cases per day") + 
      coord_cartesian(xlim = c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
      scale_fill_manual(values = colors_weekday)
    
    return(ggarrange(r_plot, cases_plot, ncol = 1, nrow = 2, heights = c(2, 0.9), align = "v"))
  }
  
  data_result <- readRDS("data/data.rds")
  
  output$combinedRplot <- renderPlot({
    data <- data_result[[input$county]][[input$tau - 2]]
    plotRcombined(data, input$intervention_date, input$plot_ages)
  })
  
  output$last_update <- renderText({
    data <- data_result[[input$county]][[input$tau - 2]]
    format(tail(data$dates, 1), format="%Y-%m-%d")
  })
}

ui <- fluidPage(
  withMathJax(),
  fluidRow(
    column(12, class = "col-lg-6 col-lg-offset-4", style="margin-bottom: 1em; text-align:center;",
      titlePanel("Estimation of the Time-Varying Reproduction Number \\(R_t\\) and the Time-Delay from 
          Infection to Estimation in Austria"
      )
    )
  ),
  fluidRow(
    column(4, class = "col-lg-2 col-lg-offset-2", style="",
           h3("Settings"),
      tags$div(style="background-color: #F8F8F8; border: 1px solid #95b2b7; padding:1em; padding-bottom:2em;",
       selectInput("county", "State", 
                   choices = list("Whole country" = "AT", 
                                  "Vienna" = "W",
                                  "Lower Austria" = "NÖ",
                                  "Upper Austria" = "OÖ",
                                  "Styria" = "ST",
                                  "Tyrol" = "T",
                                  "Carinthia" = "K",
                                  "Salzburg" = "S",
                                  "Vorarlberg" = "V",
                                  "Burgenland" = "B"), 
                   selected = "AT"),
       dateInput("intervention_date", "Infection date", ymd("2020-04-01")),
       sliderInput(inputId = "tau", label = "Sliding time window \\(\\tau\\) [days]", 
                   min = 3,
                   max = 20,
                   value = 7),
       checkboxInput("plot_ages", "Plot AGES \\(\\tau = 13\\) days \\(R_{t,\\tau}\\) estimate", FALSE),
       strong("Last update:"),
       textOutput("last_update")
      )
    ),
    column(8,class = "col-lg-6",
       plotOutput(outputId = "combinedRplot", height="70vh") %>% withSpinner(color="#4c666f"),
       
       tags$h3("Time Delay from Infection to Estimation"),
       tags$p("The color gradient in the background of the plot shows the estimated cumulative density function of 
       the time delay from infection date selected to the inclusion in the \\(R_{t,\\tau}\\) estimate."),
       
       tags$h3("Source Code and Methods"),
       tags$p("This tool is open source under an Apache License 2.0 and available on ", 
              tags$a(href="https://github.com/fvalka/r_estimate", "GitHub"), "."),
       tags$p(
         "Methods used are described in ",
         tags$a(href="https://fvalka.github.io/r_estimate/r_estimate-methods.pdf", "Estimation and Interactive Visualization of the Time-Varying 
         Reproduction Number \\(R_t\\) and the Time-Delay from Infection to Estimation"), "."
       ),
       h3("Data sources"),
       h4("Case data for Austria"),
       tags$div(checked=NA,
                tags$a(href="https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html", 
                       "Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz"),
                tags$br(),
                "aggregated by the",
                tags$a(href="https://www.csh.ac.at/", "Complexity Science Hub Vienna"),
                " and published on ",
                tags$a(href="https://github.com/osaukh/dashcoch-AT", "GitHub"),
                ".",
                tags$br(),
                "The data as published at the 15:00 CET deadline is used in this dataset."
       ),
       h4("AGES \\(R_{t,\\tau}\\) estimate"),
       tags$div(checked=NA, style="margin-bottom: 3em;",
                tags$a(href="https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/", 
                       "AGES - Österreichische Agentur für Gesundheit und Ernährungssicherheit GmbH")
       )
    )   
  )
)

shinyApp(ui = ui, server = server)