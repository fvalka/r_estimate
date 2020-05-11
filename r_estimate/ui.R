require(shiny)
require(ggplot2)
require(lubridate)
require(utils)
require(ggpubr)
require(ggnewscale)
require(latex2exp)
require(pracma)
require(shinycssloaders)
require(Cairo)
options(shiny.usecairo=T)

ui <- fluidPage(theme="yeti.css",
  withMathJax(),
  fluidRow(
    column(12, class = "col-lg-6 col-lg-offset-4", style="margin-bottom: 1em; text-align:center;",
           titlePanel("Investigation and Visualization of the Time-Delays Inherent to the Estimation of the Time-Varying 
           Reproduction Number \\(R_t\\) as Published for Austria"
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
           tags$p("Own estimates of \\(R_{t,\\tau}\\) are shown as 50% (blue) and 95% (dark blue) credible intervals. The cumulative distribution function 
           of the time-delay from the selected infection date to estimation is shown in the color gradient background (yellow to blue), 
           while the red arrow shows the 50% credible interval of the time-delay CDF."),
           
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
           h3("Data Sources"),
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
