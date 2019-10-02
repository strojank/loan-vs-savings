# Shiny app

library(shiny)
library(plotly)
library(tidyverse)
source('pomozneFunkcije.R')
options(scipen = 999)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("When to take a loan?"),
  
  # Sidebar with inputs.
  sidebarLayout(
    sidebarPanel(
      helpText(
        "This Shiny app can help you answer the following question: When in the next few years is the best time to take a loan for an investment? Calculation is based on your current funds, yearly savings, interest rates and more."
      ),
      sliderInput(
        "leta",
        "Loan maturity:",
        min = 2,
        max = 30,
        value = 10
      ),
      
      numericInput(
        "eur",
        "Estimated EURIBOR interest rate (%):",
        0,
        min = -1,
        max = 100
      ),
      numericInput(
        "bank",
        "Bank interest rate (%):",
        3,
        min = 0,
        max = 100
      ),
      numericInput(
        "exp",
        "Estimated yearly price increase of investment (%):",
        4,
        min = 0,
        max = 100
      ),
      numericInput(
        "inv",
        "Current investment estimate (€):",
        200000,
        min = 0,
        max = 10000000
      ),
      sliderInput(
        "funds",
        "Current funds (€):",
        value = 10000,
        min = 0,
        max = 200000,
        step = 1000
      ),
      sliderInput(
        "savings",
        "Average yearly savings (€):",
        value = 10000,
        min = 0,
        max = 100000,
        step = 1000
      )
      
      
      
    ),
    
    
    mainPanel(tabsetPanel(
      # Loans and interests
      tabPanel(
        "Loan and interests",
        h4("Funds, loan and interests"),
        p(
          "The plot shows you the cost of total investment separated to your funds, loan and the cost of that loan (interests) depending on the year you decide to invest. You can hoover on top of the chart to display values"
        ),
        plotlyOutput("loanPlot"),
        tags$hr()
      ),
      # Total investment
      tabPanel(
        "Total investment",
        h4("Total investment cost"),
        p(
          "The plot shows you total cost of investment depending on the year you decide to invest."
        ),
        plotlyOutput("investmentPlot")
      ),
      # Payments
      tabPanel(
        "Payments",
        h4("Payments"),
        p(
          "In the table below you can see how much money you need to borrow, what is the cost of that loan and the value of monthly payment depending on the year you decide to invest."
        ),
        DT::dataTableOutput("loanTable")
      ),
      # Credits
      tabPanel(
        "Credits",
        tags$hr(),
        HTML(
          "Loan and payments calculations are based on <a href='https://github.com/lcolladotor/mortgage'>Mortage Calculator</a> by <a href='http://bit.ly/LColladoTorres'>L. Collado Torres</a>."
        ),
        tags$hr(),
        HTML(
          "Powered by <a href='http://www.rstudio.com/shiny/'>Shiny</a> and hosted by <a href='http://www.rstudio.com/'>RStudio</a>."
        ),
        tags$hr(),
        HTML(
          "Developed by  <a href='https://twitter.com/strojanklemen'>Klemen Strojan.</a>"
        ),
        tags$hr(),
        HTML(
          "Code hosted by <a href='https://github.com/strojank/loan-vs-savings'>GitHub</a>."
        ),
        tags$hr()
        
      )
      
    ))
    
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # loan data calcualtion
  investmentData <-
    reactive({
      cenaInvesticije(
        loanDuration = input$leta,
        euribor = input$eur,
        bankInterest = input$bank,
        investmentEstimate = input$inv,
        priceIncrease = input$exp / 100,
        currentFunds = input$funds,
        yearlySavings = input$savings
      )
    })
  # subseting table data, formating
  tableData <- reactive({
    investmentData() %>%
      select(startYear,
             loan,
             interests,
             payment) %>%
      datatable() %>%
      formatCurrency(
        c('loan',
          'interests',
          'payment'),
        currency = "€",
        interval = 3,
        mark = ".",
        dec.mark = ',',
        digits = 2
      )
    
  })
  
  # subseting main results data
  loanData <- reactive({
    investmentData() %>%
      pivot_longer(cols = -c(startYear, totalInvestment)) %>%
      filter(name %in% c('funds', 'loan', 'interests'))
  })
  
  # subseting investment plot data
  invPlotData <- reactive({
    investmentData() %>%
      pivot_longer(cols = -startYear) %>%
      filter(name %in% c('totalInvestment'))
  })

    # Funds, loan and interests plot
  output$investmentPlot <- renderPlotly({
    ggplot(invPlotData(),
           aes(
             x = startYear,
             y = value,
             color = name,
             fill = name,
             label = value
           )) +
      geom_bar(stat = 'identity') +
      labs(x = 'Starting year',
           y = 'EUR',
           title = '') +
      scale_y_continuous(labels = scales::dollar_format(suffix = "", prefix = "€")) +
      scale_color_manual(values = c("#000000")) +
      scale_fill_manual(values = c("#000000")) +
      theme_bw()
    
  })
  # investment plot
  output$loanPlot <- renderPlotly({
    ggplot(
      loanData(),
      aes(
        x = startYear,
        y = value,
        color = name,
        fill = name,
        label = totalInvestment
      )
    ) +
      geom_bar(stat = 'identity') +
      labs(x = 'Starting year',
           y = 'EUR') +
      scale_y_continuous(labels = scales::dollar_format(suffix = "", prefix = "€")) +
      scale_fill_manual(values = c("#740001", "#eeba30", "#ae0001")) +
      scale_color_manual(values = c("#000000", "#000000", "#000000")) +
      theme_bw()
    
    
  })
  # Payments table
  output$loanTable <- DT::renderDataTable({
    tableData()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
