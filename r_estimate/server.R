require(shiny)
require(ggplot2)
require(lubridate)
require(utils)
require(ggpubr)
require(ggnewscale)
require(latex2exp)
require(pracma)
require(Cairo)
options(shiny.usecairo=T)

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
  
  plotRcombined <- function(data, infection_date, plot_ages) {
    
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
    delay_ecdf <- movavg(ecdf_incubation_reporting(difftime(dates_all_plot_extended, infection_date , units = c("days")) + 0.5), 
                         n=data$window_size, type = "s")
    delay_ecdf_plot_data <- data.frame("Start" = dates_all_plot_extended, "End" = dates_all_plot_extended + days(1), 
                                       "Delay CDF" = delay_ecdf)
    
    
    # Workaround: Setting x causes a warning, but not setting x causes an error
    r_plot <- r_plot + geom_rect(data = delay_ecdf_plot_data, aes(x=Start, xmin=Start, xmax=End, ymin=0, ymax=10, fill=Delay.CDF), color=NA, alpha=1.0) +
      scale_fill_gradient2(low="white", mid="#f9f6d4", high="#a5efee", midpoint=0.5, limits=c(0,1)) +
      labs(fill=TeX("time-delay CDF"))
    
    colors <- c("Own" = "#264653", "AGES" = "#e76f51")
    
    r_plot <- r_plot +
      new_scale_fill() +
      geom_ribbon(aes(ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`, fill="Own"), alpha=0.4) + 
      geom_ribbon(aes(ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`), alpha=0.5, fill="#5f7e87") + 
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5) +
      geom_vline(xintercept = infection_date, color="#d66449", alpha=0.3, size=2) +
      annotate(geom = "text", x = infection_date, y = 3.0, label="Infection", color="#822c20", hjust = 0.5, vjust=-0.4, angle = 90) +
      xlab("Date") +
      ylab(TeX(paste("$R_{t,\\tau}$ with sliding time window $\\,\\tau =", data$window_size, "$ days"))) +
      coord_cartesian(ylim = c(0, 3.5), xlim=c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
      grid() +
      theme_pubr() +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Source") + 
      theme(plot.caption=element_text(hjust=0)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)
    
    if(plot_ages) {
      r_plot <- r_plot + geom_line(data = data$ages_estimate, aes(x=ymd(Datum), y=R_eff), alpha=0.7, color="#e76f51") +
        geom_ribbon(data = data$ages_estimate, mapping =  aes(x=ymd(Datum), ymin=R_eff_lwr, ymax=R_eff_upr, fill="AGES"), alpha=0.5)
    }
    
    # add time-delay labeling
    time_delay_arrow_coordinates <- data.frame("Start" = delay_ecdf_plot_data$Start[min(which(delay_ecdf_plot_data$Delay.CDF >= 0.25))], 
                                               "Mid" = delay_ecdf_plot_data$Start[min(which(delay_ecdf_plot_data$Delay.CDF > 0.5))], 
                                               "End" = delay_ecdf_plot_data$End[min(which(delay_ecdf_plot_data$Delay.CDF >= 0.75))])
    
    r_plot <- r_plot +
      annotate(geom="text", x=time_delay_arrow_coordinates$Mid, y=3.0, 
               label="atop(inclusion,\"in estimate\")", color="#822c20", 
               hjust = 0.5, vjust=0.5, alpha=0.6, parse=TRUE) +
      geom_segment(data = time_delay_arrow_coordinates,
                   aes(x=Start, 
                       xend=End, 
                       y=3.0, yend=3.0), 
                   size = 0.5, colour="#822c20", alpha=0.4,
                   arrow = arrow(length = unit(0.3, "cm")))
    
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