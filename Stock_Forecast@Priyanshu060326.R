library(shiny)
library(shinydashboard)
library(quantmod)
library(forecast)
library(plotly)
library(rugarch)
library(TTR)

ui <- dashboardPage(
  
  dashboardHeader(title = "Stock Forecast Dashboard"),
  
  dashboardSidebar(
    
    selectInput(
      "symbol",
      "Select Stock",
      choices = c("AAPL","GOOGL","MSFT","AMZN","TSLA"),
      selected = "AAPL"
    ),
    
    numericInput(
      "days",
      "Forecast Days",
      value = 30,
      min = 1,
      max = 365
    )
    
  ),
  
  dashboardBody(
    
    fluidRow(
      box(
        title = "Stock Price Trend",
        width = 12,
        plotlyOutput("pricePlot")
      )
    ),
    
    fluidRow(
      
      box(
        title = "Moving Average",
        width = 6,
        plotlyOutput("maPlot")
      ),
      
      box(
        title = "ARIMA Forecast",
        width = 6,
        plotlyOutput("forecastPlot")
      )
      
    ),
    
    fluidRow(
      box(
        title = "Volatility (GARCH)",
        width = 12,
        plotlyOutput("volatilityPlot")
      )
    )
    
  )
)

server <- function(input, output) {
  
  stockData <- reactive({
    
    data <- getSymbols(
      input$symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    price <- Cl(data)
    
    return(price)
    
  })
  
  
  # STOCK PRICE GRAPH
  output$pricePlot <- renderPlotly({
    
    price <- stockData()
    
    plot_ly(
      x = index(price),
      y = as.numeric(price),
      type = "scatter",
      mode = "lines",
      name = "Price"
    ) %>%
      layout(title = "Stock Price Trend",
             xaxis = list(title="Date"),
             yaxis = list(title="Price"))
    
  })
  
  
  # MOVING AVERAGE GRAPH
  output$maPlot <- renderPlotly({
    
    price <- stockData()
    
    ma20 <- SMA(price, 20)
    ma50 <- SMA(price, 50)
    
    plot_ly() %>%
      
      add_lines(
        x = index(price),
        y = as.numeric(price),
        name = "Price"
      ) %>%
      
      add_lines(
        x = index(ma20),
        y = as.numeric(ma20),
        name = "MA20"
      ) %>%
      
      add_lines(
        x = index(ma50),
        y = as.numeric(ma50),
        name = "MA50"
      ) %>%
      
      layout(title = "Moving Average")
    
  })
  
  
  # ARIMA FORECAST
  output$forecastPlot <- renderPlotly({
    
    price <- stockData()
    
    # Convert to numeric
    price_vec <- as.numeric(price)
    
    # Fit ARIMA
    model <- auto.arima(price_vec)
    
    fc <- forecast(model, h = input$days)
    
    # Historical dataframe
    actual_df <- data.frame(
      Date = index(price),
      Price = price_vec
    )
    
    # Create future dates
    last_date <- max(index(price))
    
    future_dates <- seq(
      last_date + 1,
      by = "days",
      length.out = input$days
    )
    
    forecast_df <- data.frame(
      Date = future_dates,
      Forecast = as.numeric(fc$mean)
    )
    
    # Plot
    plot_ly() %>%
      
      add_lines(
        data = actual_df,
        x = ~Date,
        y = ~Price,
        name = "Actual Price",
        line = list(color = "blue")
      ) %>%
      
      add_lines(
        data = forecast_df,
        x = ~Date,
        y = ~Forecast,
        name = "Forecast",
        line = list(color = "orange")
      ) %>%
      
      layout(
        title = "ARIMA Forecast",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price")
      )
    
  })
  
  # GARCH VOLATILITY
  output$volatilityPlot <- renderPlotly({
    
    price <- stockData()
    
    returns <- dailyReturn(price)
    
    spec <- ugarchspec(
      variance.model = list(model="sGARCH", garchOrder=c(1,1)),
      mean.model = list(armaOrder=c(1,1))
    )
    
    fit <- ugarchfit(spec, returns)
    
    vol <- sigma(fit)
    
    plot_ly(
      x = index(vol),
      y = as.numeric(vol),
      type = "scatter",
      mode = "lines",
      name = "Volatility"
    ) %>%
      layout(title="Stock Volatility (GARCH)")
    
  })
  
}

shinyApp(ui, server)
