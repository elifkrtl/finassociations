# Finassociations: Investment Recommendation Application
# Author: Elif KARTAL, M. Erdal BALABAN, & Zeki OZEN
# Date: September 2024
# File: ui.R

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel(
    "Finassociations: Investment Recommendation Application with Association Rule Mining"
  ),
  "by Elif KARTAL, M. Erdal BALABAN, and Zeki OZEN (2024)",
  sidebarLayout(
    sidebarPanel(
      "Finassociations is a flexible and interactive investment recommendation application that enables investors to discover financial relations between currencies, cryptocurrencies and stocks through Association Rule Mining (ARM).", 
      tags$br(), 
      "Before start, you can read the Help Section to learn more about the Finassociations App and ARM. After data selection, please navigate using Next and Prev buttons on the bottom of the left panel. If you need more financial instruments (currency, cryptocurrency, or stock market data) to analyse, please make a ",
      mailtoR(email = "elifk@istanbul.edu.tr", text = "request!"),
      use_mailtoR(),
      tags$hr(),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        text = app_jscode,
        functions = c("disableTab", "enableTab")
      ),
      shinyjs::inlineCSS(app_css),
      tags$span(htmlOutput("dateInterval"), style = "color:red"),
      tabsetPanel(
        id = "myTabs",
        tabPanel(
          title = "Data Selection",
          value = "s2",
          selectInput(
            inputId = "date_intrvl",
            label = "Choose A Date Interval:",
            
            choices = c(
              "Last week" = 1,
              "Last month" = 2,
              "Last 3 months" = 3,
              "Last 6 months" = 4,
              "Last 1 year" = 5,
              "Last 2 years" = 6
            ),
            selected = 2
          ),
          multiInput(
            inputId = "cur_id",
            label = "Currencies:",
            choices = my_cursInfo,
            selected = c("EURUSD=X", "GBPUSD=X", "JPYUSD=X", "TRYUSD=X")
          ),
          multiInput(
            inputId = "crypto_id",
            label = "Cryptocurrencies:",
            choices = cryptInfo,
            selected = c("BTC-USD", "ETH-USD")
          ),
          multiInput(
            inputId = "stock_id",
            label = "Stock Markets:",
            choices = stMarInfo,
            selected = c("XU100.IS", "^DJI", "GC=F")
          ),
          selectInput(
            inputId = "tSGraph",
            label = "Line Chart for:",
            choices = c(
              "EURUSD=X",
              "GBPUSD=X",
              "JPYUSD=X",
              "TRYUSD=X",
              "BTC-USD",
              "ETH-USD",
              "XU100.IS",
              "^DJI",
              "GC=F"
            )
          ),
          div(style = "display:inline-block", actionButton("next1", "Next"))
        ),
        
        tabPanel(
          title = "Association Rule Mining",
          value = "s3",
          h5("The Rate of Change (%):"),
          div(
            style = "display: inline-block;vertical-align:top; width: 200px;",
            selectInput(
              inputId = "changeRate_down",
              label = "The Lower Bound:",
              choices = c(
                "0%" = 0,
                "-0.05%" = 0.05,
                "-0.1%" = 0.1,
                "-0.2%" = 0.2,
                "-1%" = 1,
                "-2%" = 2,
                "-3%" = 3
              )
            ),
            selected = c("0%")
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 200px;",
            
            selectInput(
              inputId = "changeRate_up",
              label = "The Upper Bound:",
              choices = c(
                "0%" = 0,
                "0.05%" = 0.05,
                "0.1%" = 0.1,
                "0.2%" = 0.2,
                "1%" = 1,
                "2%" = 2,
                "3%" = 3
              ),
              selected = c("0%")
            )
          ),
          sliderInput(
            "minConf",
            "The Minimum Confidence:",
            min = 0,
            max = 1,
            value = 0.3,
            step = 0.01
          ),
          sliderInput(
            "minSup",
            "The Minimum Support:",
            min = 0,
            max = 1,
            value = 0.3,
            step = 0.01
          ),
          numericInput(
            inputId = "minRL",
            label = "The Minimum Rule Length:",
            min = 2,
            max = 10,
            value = 2,
            step = 1
          ),
          div(style = "display:inline-block", actionButton("prev", "Prev")),
          div(style = "display:inline-block", actionButton("next2", "Get Rules!"))
          
        )
      )
    ),
    
    mainPanel(tabsetPanel(
      id = "myTabs2",
      tabPanel(
        title = "Data Selection",
        value = "m2",
        textOutput("my_alert"),
        tags$head(
          tags$style("#my_alert{color: red;
                                  font-size: 20px;
                                  font-style: italic;
                                  }")
        ),
        conditionalPanel(
          condition = "output.my_alert != 'Please select a minimum of 2 investment tools!'",
          DT::dataTableOutput("dataSelection"),
          plotOutput("cors1"),
          plotlyOutput("lp")
        )
      ),
      tabPanel(
        title = "Association Rules",
        value = "m3",
        
        conditionalPanel(
          condition = "input.next2 != 0",
          htmlOutput("RoC"),
          DT::dataTableOutput("dataSelection3"),
          htmlOutput("RoC_stat"),
          DT::dataTableOutput("dataSelection4"),
          verbatimTextOutput("ress2"),
          conditionalPanel(
            condition = "output.ress2 != 'No sufficient financial assocations! Please try again with different parameters.'",
            htmlOutput("fassocs"),
            DT::dataTableOutput("my_assocRules"),
            visNetworkOutput("ruleGraph"),
            plotOutput("mostFreqBar")
          )
        )
      ),
      tabPanel(title = "Help",
               value = "m4",
               "Finassociations: Investment Recommendation Application",
               br(),br(),
               "This application was developed as part of a scientific study entitled",a("'Investment analytics using association rule mining (Finassociations)'", href="https://www.inderscience.com/info/ingeneral/forthcoming.php?jcode=ijcee", target="_blank"), "by Kartal, E., Balaban, M. E., and Özen, Z., and has been accepted for publication in the International Journal of Computational Economics and Econometrics Special Issue on 'Economic Analysis and the Current Real-World Situation: Exploring New Trends in Applied Economics,' in honor of Prof. George Agiomirgianakis.", br(),br(),
               htmlOutput("helpFile"))
    ))
  )
)