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
  ecdf_incubation_reporting_precalculated <- readRDS("data/time-delay/infection_reporting_cdf.rds")
  ecdf_incubation_estimation_precalculated <- readRDS("data/time-delay/infection_estimation_cdf.rds")
  
  ecdf_incubation_reporting <- Vectorize(function(t) {
    idx <- findInterval(t, ecdf_incubation_reporting_precalculated$t, all.inside=TRUE)
    return(ecdf_incubation_reporting_precalculated$infection_reporting_cdf[idx])
  })
  
  ecdf_incubation_estimation <- Vectorize(function(t, tau) {
    idx <- findInterval(t, ecdf_incubation_estimation_precalculated$t, all.inside=TRUE)
    return(ecdf_incubation_estimation_precalculated[[sprintf("tau_%d", tau)]][idx])
  })
  
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
  
  plotRcombined <- function(data, infection_date, plot_ages, tau) {
    
    first_date <- head(data$dates, 1)
    last_date <- tail(data$dates, 1)
    
    first_plot_date <- first_date + days(7)
    
    dates_all_plot <- first_plot_date + days(0:(last_date-first_plot_date))
    dates_all_plot_extended <- first_date + days(0:((last_date-first_plot_date)+10))
    
    cases_with_dates <- data.frame("Date" = data$dates, "Cases" = data$cases, 
                                   "Weekday" = date_to_weekday(data$dates))
    
    r_plot <- ggplot(data = data$estimated_R, aes(x=data$case_incidence$dates[t_end]))
    
    # Add half a day to obtain the midpoint between t_Start and t_End
    delay_ecdf <- ecdf_incubation_estimation(difftime(dates_all_plot_extended, infection_date , units = c("days")), tau)
    delay_ecdf_plot_data <- data.frame("Start" = dates_all_plot_extended, "End" = dates_all_plot_extended + days(1), 
                                       "Delay CDF" = delay_ecdf)
    
    
    # Workaround: Setting x causes a warning, but not setting x causes an error
    r_plot <- r_plot + geom_rect(data = delay_ecdf_plot_data, aes(x=Start, xmin=Start, xmax=End, ymin=0, ymax=10, fill=Delay.CDF), 
                                 color=NA, alpha=1.0) +
      scale_fill_gradient2(low="white", mid="#f9f6d4", high="#a5efee", midpoint=0.5, limits=c(0,1)) +
      labs(fill=TeX("Time-delay CDF"))
    
    colors <- c("Own" = "#264653", "AGES" = "#e76f51")
    
    r_plot <- r_plot +
      new_scale_fill() +
      geom_ribbon(aes(ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`, fill="Own"), alpha=0.4) + 
      geom_ribbon(aes(ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`), alpha=0.5, fill="#5f7e87") + 
      geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.5) +
      geom_vline(xintercept = infection_date, color="#d66449", alpha=0.3, size=2) +
      annotate(geom = "text", x = infection_date, y = 3.0, label="Infection", color="#822c20", hjust = 0.5, vjust=-0.4, angle = 90) +
      xlab("Reporting date") +
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
      xlab("Reporting date") +
      ylab("New cases per day") + 
      coord_cartesian(xlim = c(head(dates_all_plot, 1), tail(dates_all_plot, 1))) +
      scale_fill_manual(values = colors_weekday, limits=c("Workday", "Weekend", "Holiday"))
    
    return(ggarrange(r_plot, cases_plot, ncol = 1, nrow = 2, heights = c(2, 0.9), align = "v"))
  }
  
  plotEcdfs <- function(tau) {
    t <- seq(0, 30)
    t_fine <- seq(0,30, 0.1)
    
    # Obtained from the supplementary appendix of 
    # .Zhang, J. et al. Evolving epidemiology and transmission dynamics of coronavirus disease 2019 outside Hubei province, China: 
    # a descriptive and modelling study. The Lancet Infectious Diseases 0, (2020).
    ecdf_infection_onset <- pgamma(t_fine, shape = 4.23, rate = 0.81) # incubation period
    
    ecdf_incubation_estimation_plot <- data.frame("t"=t, 
                                 "infection_estimation"=ecdf_incubation_estimation(t, tau))
    ecdfs_for_plot <- data.frame("t" =t_fine,
                                "infection_onset" = ecdf_infection_onset,
                                "infection_reporting"=ecdf_incubation_reporting(t_fine))
    
    colors <- c("onset" = "#c9b36a", "reporting" = "#e76f51", "estimation" = "#66aaa8")
    
    # Add half a day to obtain the midpoint between t_Start and t_End
    delay_ecdf <- ecdf_incubation_estimation(t, tau)
    delay_ecdf_plot_data <- data.frame("Start" = t, "End" = t + 1, 
                                       "Delay CDF" = delay_ecdf)
    
    plot_result <- ggplot(data=ecdfs_for_plot, aes(x=t))+ 
      geom_rect(data = delay_ecdf_plot_data, aes(x=Start, xmin=Start, xmax=End, ymin=0, ymax=1, fill=Delay.CDF), color=NA, alpha=1.0) +
      geom_ribbon(data=ecdf_incubation_estimation_plot, aes(ymin=infection_estimation, ymax=1), fill="white", alpha=1.0) + 
      scale_fill_gradient2(low="white", mid="#f9f6d4", high="#a5efee", midpoint=0.5, limits=c(0,1)) +
      labs(fill=TeX("Time-delay CDF")) +
      geom_line(aes(x=t_fine, y=infection_onset, color="onset"), size=0.9, alpha=0.7) +
      geom_line(aes(x=t_fine, y=infection_reporting, color="reporting"), size=0.9, alpha=0.7) +
      geom_line(data=ecdf_incubation_estimation_plot, 
                aes(x=t, y=infection_estimation, color="estimation"), size=0.9, alpha=0.7) +
      geom_hline(yintercept=0.25, linetype="dashed", color = "black", size=0.5, alpha=0.5) +
      geom_hline(yintercept=0.75, linetype="dashed", color = "black", size=0.5, alpha=0.5) +
      theme_pubr() +
      xlab("Days after infection") +
      ylab("Pr") +
      labs(color = "Infection to") +
      scale_color_manual(values = colors, limits=c("onset", "reporting", "estimation"))
    
    return(plot_result)
  }
  
  tableLatestR <- function(data) {
    row <- tail(data$estimated_R, 1)
    formated_result <- data.frame(
      "Credible Interval" = c("50%", "95%"),
      "Lower" = c(row$`Quantile.0.25(R)`, row$`Quantile.0.025(R)`),
      "Upper" = c(row$`Quantile.0.75(R)`, row$`Quantile.0.975(R)`))
    return(formated_result)
  }
  
  data_result <- readRDS("data/data.rds")
  
  output$combinedRplot <- renderPlot({
    data <- data_result[[input$county]][[input$tau - 2]]
    plotRcombined(data, input$intervention_date, input$plot_ages, input$tau)
  })
  
  output$ecdfsPlot <- renderPlot({
    plotEcdfs(input$tau)
  })
  
  output$last_update <- renderText({
    data <- data_result[[input$county]][[input$tau - 2]]
    format(tail(data$dates, 1), format="%Y-%m-%d")
  })
  
  output$testTbl <- renderTable({ 
    data <- data_result[[input$county]][[input$tau - 2]]
    tableLatestR(data)
  }, digits = 2)  
}