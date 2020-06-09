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
  tags$head(
    tags$meta(property="og:title", content="Investigation and Visualization of the Time-Delays Inherent to the Estimation of the Time-Varying 
           Reproduction Number R(t) as Published for Austria"),
    tags$meta(property="og:image", content="https://covid19-r.com/social-media-preview-20200518.png")
  ),
  tags$script(src = "webanalytics.js"),
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
                    tags$label("Country"),
                    tags$p("Austria"),
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
                                value = 13),
                    checkboxInput("plot_ages", "Plot AGES \\(\\tau = 13\\) days \\(R_{t,\\tau}\\) estimate", FALSE),
                    strong("Last update:"),
                    textOutput("last_update")
           )
    ),
    column(8,class = "col-lg-6",
           plotOutput(outputId = "combinedRplot", height="70vh") %>% withSpinner(color="#4c666f"),
           tags$p("Own estimates of \\(R_{t,\\tau}\\) are shown as 50% (blue) and 95% (dark blue) credible intervals. 
           The cumulative distribution function 
           of the time-delay from the selected infection date to estimation is shown in the color gradient background (yellow to blue), 
           while the red arrow shows the 50% credible interval of the time-delay CDF."),
           
           tags$h3("Warning & Disclaimer"),
           tags$p("Please note that this is currently a",
              tags$strong("work in progress."), 
              "Which has not been peer-reviewed yet. Any estimates shown here might be wrong. Our own estimates and estimates for the 
              time delays are neither issued nor endorsed by AGES, the Austrian government or any universities or companies the authors might be
              affiliated with.
              Use at your own discretion and your own risk."),
           
           tags$h3("Time Delay from Infection to Estimation"),
           tags$p("The color gradient in the background of the plot shows the estimated cumulative density function of 
           the time delay from the infection date selected in the settings to the inclusion in the \\(R_{t,\\tau}\\) estimate."),
           
           tags$h4("Different Time Delays and Sources of the Time Delays"),
           tags$p("We consider three different time estimates which are involved in the estimation of the time-varying reproduction number \\(R_t\\)."),
           tags$p("The first one is the time-delay from infection to the onset of symptoms, also called the incubation time, \\(t_{infection,onset}\\).
                  Estimates for the Gamma distribution as published in China are used for this. For details and sources please refer to the",
                  tags$a(href="https://fvalka.github.io/r_estimate/r_estimate-methods.pdf", "methods paper.")),
           
           tags$p("The second time-delay we consider is the time-delay from symptom onset to official reporting, \\(t_{onset,reporting}\\).
                  For this time-delay also an estimate for it's Gamma distribution parameters from China is used. For details and sources please also refer to the",
                  tags$a(href="https://fvalka.github.io/r_estimate/r_estimate-methods.pdf", "methods paper.")),
           
           tags$p("Finally another time-delay has to be considered. The \\(R_{t,\\tau}\\) estimation methods assumes that the time-varying reproduction number
                  \\(R_t\\) is constant within the sliding time-window \\(\\tau\\). This also introduces an additional time-delay until the change in transmission 
                  can be seen in the \\(R_{t,\\tau}\\) estimate. This effect can be compared to appyling a moving-average over a window of length \\(\\tau\\) 
                  although the actual time-delay introduced depends on the serial interval distribution and leads to a more complicated time-response of the
                  \\(R_{t,\\tau}\\) estimate."),
           tags$p("We investigated this time-delay in detail, also as described in more detail in the methods paper, and included it in this estimation here. 
                  You can also see that adjusting \\(\\tau\\) changes the time-delay cumulative distribution function shown in the main figure and also shown below."),
           tags$p("All of these time-delays are combined to obtain an estimate of the total time-delay from infection to estimation \\(t_{infection,estimation}\\).
                  The result is also itself a probability distribution."),
           
           tags$h4("Cumulative Distribution Function from Infection to Estimation"),
           tags$p("This figure shows the estimated cumulative distribution functions for the time delays from
           infection to onset, reporting, and estimation: 
                  \\(t_{infection,onset}\\) (yellow), \\(t_{infection,reporting}\\) (red), 
                  and \\(t_{infection,estimation}\\) (blue) for the currently selected value of \\(\\tau\\)."),
           tags$div(style="margin-bottom: 1em; margin-top: 1em;",
              plotOutput(outputId = "ecdfsPlot") %>% withSpinner(color="#4c666f")
           ),
           tags$p("In this figure the 0.25 and 0.75 quantiles are also marked with dashed lines, signifying 
                  the 50% credible interval."),
           
           tags$h3("Latest \\(R_{t,\\tau}\\) Estimate for Currently Selected Parameters"),
           tags$p("Credible intervals for the \\(R_{t,\\tau}\\) estimate are obtained from the equal-tailed intervals.
                  Given the z-quantile \\(q_z\\), the 95% credible interval is obtained from the 95% equal-tailed interval
                  \\(I_{0.05} = \\left[q_{0.025}, q_{0.975}\\right]\\)."),
           tags$div(style="display: flex; justify-content: center;",
                    tableOutput('testTbl')),
           
           tags$h3("Interpretation"),
           tags$h4("Problems with using Confirmed Cases"),
           tags$p("There is probably a general systematic error introduced by time-dependent ascertainment. This means that since a different number of people have been
                  tested for each known infection at different times during the pandemic and also testing strategies might have changed during the course of the spread.
                  This introduces a bias in the \\(R_{t,\\tau}\\) estimation during times of changing ascertainment."),
           tags$h4("The Role of Time-Delays"),
           tags$p("Time-delays play a key role when we want to consider whether the effects of a change, which was implemented at a specific date is already visible
                  in the resulting estimate of the time-varying reproduction number \\(R_{t,\\tau}\\)."),
           tags$p("This delay is also of integral importance when we consider the problem of changing interventions from the viewpoint of control theory where any 
                  additional time-delay between the output (the changes in transmission as reflected in the \\(R_{t,\\tau}\\) estimate) and the controller (us) can introduce
                  instability into an otherwise stable control-loop."),
           tags$p("Our approach here is basically the inverse of a nowcasting estimate, 
                  which was chosen since the official estimates for Austria are published like this and also because it makes the uncertaintiy in the time-delay clearer.
                  And allows the user to choose his own probability cut-off. "),
           
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
           h4("AGES, TU Graz \\(R_{t,\\tau}\\) estimate"),
           tags$div(checked=NA, style="margin-bottom: 3em;",
                    tags$a(href="https://www.ages.at/en/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-2020/", 
                           "AGES - Österreichische Agentur für Gesundheit und Ernährungssicherheit GmbH")
           )
    )   
  ),
  tags$footer(style="margin-left: -15px; margin-right: -15px; padding-bottom: 1em; padding-left: 1em; padding-right: 1em; background-color: #F8F8F8; border-top: 1px solid #95b2b7;",
              class="text-center",
              tags$p(style="margin-top: 1em",
                tags$a(href="https://zenodo.org/badge/latestdoi/259440541", tags$img(src="https://zenodo.org/badge/259440541.svg", alt="DOI")),
                
              ),
              tags$p(tags$a(href="http://creativecommons.org/licenses/by/4.0/", tags$img(src="https://i.creativecommons.org/l/by/4.0/80x15.png", alt="CC-BY")),
                     tags$br(),
                     tags$small("Valka, Fabian and Schuler, Carla"),), 
              tags$p(tags$small("Suggested citation: Valka, Fabian & Schuler, Carla. Investigation and Visualization of the Time-Delays Inherent to the Estimation of 
                the Time-Varying Reproduction Number Rt as Published for Austria. (2020). doi:10.5281/zenodo.3841756")),
              tags$p(tags$small(tags$a(href="privacy.html", "Imprint & Privacy Policy")))
  )
)
