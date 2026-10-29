# BDA400 Assignment 6
# Technical Analysis Dashboard
# Student: Raya Alasouad

# Step 1: Load libraries
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(gridExtra)

# Step 2: Dashboard layout
ui <- fluidPage(
  titlePanel("Stock Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", "AAPL"),
      
      dateRangeInput(
        "dates", "Select Date Range",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      
      selectInput(
        "frame", "Time Frame",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "chart", "Chart Type",
        choices = c("Line", "Area", "Candlestick")
      ),
      
      checkboxGroupInput(
        "indicators", "Technical Indicators",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      
      numericInput("short", "Short MA", 20, min = 2),
      numericInput("long", "Long MA", 50, min = 3),
      
      checkboxInput(
        "signals", "Show Buy/Sell Signals", TRUE
      )
    ),
    
    mainPanel(
      h3("Stock Price Analysis"),
      textOutput("status"),
      plotOutput("stock_chart", height = "800px"),
      h4("Latest Trading Signal"),
      textOutput("latest"),
      h4("Recent Buy and Sell Signals"),
      tableOutput("signal_table")
    )
  )
)# Step 3: Server and stock data
server <- function(input, output, session) {
  
  stock_data <- reactive({
    req(input$symbol, input$dates)
    
    tryCatch({
      getSymbols(
        toupper(trimws(input$symbol)),
        src = "yahoo",
        from = as.Date(input$dates[1]) - 800,
        to = as.Date(input$dates[2]) + 1,
        auto.assign = FALSE
      )
    }, error = function(e) NULL)
  })
  
  # Step 4: Calculate indicators
  data <- reactive({
    stock <- stock_data()
    
    validate(need(
      !is.null(stock),
      "Unable to download data. Check internet or stock symbol."
    ))
    
    stock <- switch(
      input$frame,
      "Daily" = stock,
      "Weekly" = to.weekly(stock, indexAt = "lastof"),
      "Monthly" = to.monthly(stock, indexAt = "lastof")
    )
    
    close <- as.numeric(Cl(stock))
    short_n <- as.integer(input$short)
    long_n <- as.integer(input$long)
    
    validate(need(
      short_n < long_n,
      "Short MA must be smaller than Long MA."
    ))
    
    validate(need(
      length(close) >= max(long_n, 35) + 2,
      "Not enough stock data."
    ))
    
    ma_short <- as.numeric(SMA(close, n = short_n))
    ma_long <- as.numeric(SMA(close, n = long_n))
    rsi <- as.numeric(RSI(close, n = 14))
    
    macd <- MACD(
      close,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      maType = "EMA"
    )
    
    # Step 5: Buy, Sell and Hold signals
    difference <- ma_short - ma_long
    previous <- c(NA, head(difference, -1))
    
    signal <- rep("Hold", length(close))
    
    buy <- !is.na(previous) &
      !is.na(difference) &
      previous <= 0 & difference > 0
    
    sell <- !is.na(previous) &
      !is.na(difference) &
      previous >= 0 & difference < 0
    
    signal[buy] <- "Buy"
    signal[sell] <- "Sell"
    
    df <- data.frame(
      Date = as.Date(index(stock)),
      Open = as.numeric(Op(stock)),
      High = as.numeric(Hi(stock)),
      Low = as.numeric(Lo(stock)),
      Close = close,
      ShortMA = ma_short,
      LongMA = ma_long,
      RSI = rsi,
      MACD = as.numeric(macd[, 1]),
      MACDSignal = as.numeric(macd[, 2]),
      Signal = signal
    )
    
    df <- df[
      df$Date >= as.Date(input$dates[1]) &
        df$Date <= as.Date(input$dates[2]),
      ,
      drop = FALSE
    ]
    
    validate(need(nrow(df) > 0, "No data for selected dates."))
    df
  })# Step 6: Show stock information
  output$status <- renderText({
    paste(
      "Stock:", toupper(input$symbol),
      "| Time Frame:", input$frame
    )
  })
  
  # Step 7: Create charts
  output$stock_chart <- renderPlot({
    df <- data()
    
    p <- ggplot(df, aes(x = Date)) +
      labs(
        title = "Stock Price and Trading Signals",
        x = "Date",
        y = "Stock Price"
      ) +
      theme_minimal()
    
    if (input$chart == "Line") {
      p <- p + geom_line(
        aes(y = Close), color = "blue"
      )
    }
    
    if (input$chart == "Area") {
      p <- p +
        geom_area(
          aes(y = Close),
          fill = "skyblue",
          alpha = 0.5
        ) +
        geom_line(
          aes(y = Close),
          color = "blue"
        )
    }
    
    if (input$chart == "Candlestick") {
      width <- switch(
        input$frame,
        Daily = 0.6,
        Weekly = 3.5,
        Monthly = 14
      )
      
      p <- p +
        geom_linerange(
          aes(ymin = Low, ymax = High)
        ) +
        geom_rect(
          aes(
            xmin = Date - width / 2,
            xmax = Date + width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "black"
        ) +
        scale_fill_manual(
          values = c("TRUE" = "green", "FALSE" = "red"),
          guide = "none"
        )
    }
    
    # Moving Average overlay
    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(
          aes(y = ShortMA),
          color = "orange",
          linewidth = 0.7,
          na.rm = TRUE
        ) +
        geom_line(
          aes(y = LongMA),
          color = "purple",
          linewidth = 0.7,
          na.rm = TRUE
        )
    }
    
    # Buy and Sell annotations
    if (isTRUE(input$signals)) {
      trade_points <- df[df$Signal != "Hold", ]
      
      p <- p +
        geom_point(
          data = trade_points,
          aes(y = Close, color = Signal),
          size = 3
        ) +
        geom_text(
          data = trade_points,
          aes(y = Close, label = Signal, color = Signal),
          vjust = -1,
          size = 3
        ) +
        scale_color_manual(
          values = c("Buy" = "darkgreen", "Sell" = "red")
        )
    }# Step 8: RSI and MACD charts
    charts <- list(p)
    
    if ("RSI" %in% input$indicators) {
      rsi_chart <- ggplot(df, aes(x = Date, y = RSI)) +
        geom_line(color = "purple", na.rm = TRUE) +
        geom_hline(yintercept = 70, linetype = "dashed",
                   color = "red") +
        geom_hline(yintercept = 30, linetype = "dashed",
                   color = "green") +
        labs(title = "Relative Strength Index (RSI)",
             x = "Date", y = "RSI") +
        theme_minimal()
      
      charts <- append(charts, list(rsi_chart))
    }
    
    if ("MACD" %in% input$indicators) {
      macd_chart <- ggplot(df, aes(x = Date)) +
        geom_line(aes(y = MACD), color = "blue",
                  na.rm = TRUE) +
        geom_line(aes(y = MACDSignal), color = "red",
                  na.rm = TRUE) +
        labs(title = "MACD Indicator",
             x = "Date", y = "MACD") +
        theme_minimal()
      
      charts <- append(charts, list(macd_chart))
    }
    
    do.call(grid.arrange, c(charts, ncol = 1))
  })
  
  # Step 9: Latest trading signal
  output$latest <- renderText({
    df <- data()
    latest <- tail(df, 1)
    paste("Latest Signal:", latest$Signal)
  })
  
  # Step 10: Trading signal table
  output$signal_table <- renderTable({
    df <- data()
    trades <- df[df$Signal != "Hold",
                 c("Date", "Close", "Signal")]
    
    tail(trades, 10)
  })
}

# Step 11: Start Shiny application
shinyApp(ui = ui, server = server)