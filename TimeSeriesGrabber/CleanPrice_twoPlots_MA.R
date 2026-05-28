library(shiny)
library(quantmod)
library(TTR)

get_data <- function(symbol, from) {
  data <- tryCatch(
    getSymbols(symbol, auto.assign = FALSE, from = from),
    error = function(e) NULL
  )

  if (is.null(data) || NROW(data) < 5) return(NULL)

  price <- tryCatch(Cl(data), error = function(e) NULL)
  if (is.null(price) || NROW(price) < 5) return(NULL)

  peaks <- tryCatch(
    price[price > stats::lag(price, 1) & price > stats::lag(price, -1)],
    error = function(e) NULL
  )

  troughs <- tryCatch(
    price[price < stats::lag(price, 1) & price < stats::lag(price, -1)],
    error = function(e) NULL
  )

  if (is.null(peaks) || is.null(troughs)) {
    clean <- price
  } else {
    clean <- rbind(peaks, troughs)
  }

  if (is.null(clean) || NROW(clean) < 5) {
    clean <- price
  }

  clean <- tryCatch(clean[order(index(clean))], error = function(e) price)

  list(price = price, clean = clean)
}

plot_clean_price <- function(symbol, from) {
  symbol <- toupper(trimws(symbol))
  validate(need(symbol != "", "Enter a symbol."))

  d <- get_data(symbol, from)
  validate(need(!is.null(d), paste("No valid price data for", symbol)))

  clean <- d$clean
  ma20 <- SMA(clean, n = 20)
  ma50 <- SMA(clean, n = 50)
  x <- index(clean)
  y <- as.numeric(clean)
  ma20_y <- as.numeric(ma20)
  ma50_y <- as.numeric(ma50)
  y_range <- range(c(y, ma20_y, ma50_y), na.rm = TRUE)

  par(mar = c(3, 4, 3, 1), xaxs = "i")
  plot(
    x,
    y,
    type = "l",
    main = paste(symbol, "Clean Price"),
    ylab = "Price",
    xlab = "",
    col = "black",
    lwd = 1.5,
    ylim = y_range
  )
  lines(x, ma20_y, col = "#1f77b4", lwd = 2)
  lines(x, ma50_y, col = "#d62728", lwd = 2)
  legend(
    "topleft",
    legend = c("Clean Price", "20-MA", "50-MA"),
    col = c("black", "#1f77b4", "#d62728"),
    lwd = c(1.5, 2, 2),
    bty = "n"
  )
}

clip01 <- function(x) {
  pmin(pmax(x, 0), 1)
}

get_hover_readout <- function(data, hover) {
  if (is.null(data) || is.null(hover)) return("Hover plot for values")

  clean <- data$clean
  values <- data.frame(
    date = index(clean),
    price = as.numeric(clean),
    ma20 = as.numeric(SMA(clean, n = 20)),
    ma50 = as.numeric(SMA(clean, n = 50))
  )

  if (nrow(values) < 1 || is.null(hover$x)) return("Hover plot for values")

  i <- which.min(abs(as.numeric(values$date) - as.numeric(hover$x)))
  row <- values[i, ]
  ma20_text <- ifelse(is.na(row$ma20), "NA", sprintf("%.2f", row$ma20))
  ma50_text <- ifelse(is.na(row$ma50), "NA", sprintf("%.2f", row$ma50))

  sprintf(
    "%s | P %.2f | 20-MA %s | 50-MA %s",
    format(row$date, "%Y-%m-%d"),
    row$price,
    ma20_text,
    ma50_text
  )
}

get_strategy <- function(symbol, from, ma20_weight, strategy_scale) {
  symbol <- toupper(trimws(symbol))
  if (symbol == "") return(NULL)
  ma20_weight <- clip01(as.numeric(ma20_weight))
  strategy_scale <- pmin(pmax(as.numeric(strategy_scale), 0), 10)

  d <- get_data(symbol, from)
  if (is.null(d)) return(NULL)

  clean <- d$clean
  ma20 <- SMA(clean, n = 20)
  ma50 <- SMA(clean, n = 50)
  strategy_data <- na.omit(merge(clean, ma20, ma50))

  if (NROW(strategy_data) < 1) return(NULL)

  latest <- tail(strategy_data, 1)
  p <- as.numeric(latest[, 1])
  ma20_latest <- as.numeric(latest[, 2])
  ma50_latest <- as.numeric(latest[, 3])

  s <- ma20_weight * ((p - ma20_latest) / ma20_latest) +
    (1 - ma20_weight) * ((p - ma50_latest) / ma50_latest)

  holding <- round(clip01(0.5 + strategy_scale * s), 2)
  hedging <- round(clip01(0.5 - strategy_scale * s), 2)
  cash <- round(1 - holding - hedging, 2)

  list(
    holding = holding,
    hedging = hedging,
    cash = cash
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body, .container-fluid {
        height: 100%;
        margin: 0;
        padding: 0;
      }

      .plot-half {
        height: 50vh;
        padding: 10px 14px 6px;
        border-bottom: 1px solid #ddd;
        box-sizing: border-box;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }

      .symbol-row {
        width: 220px;
        margin-bottom: 4px;
      }

      .symbol-row .form-group {
        margin-bottom: 0;
      }

      .parameter-row {
        display: flex;
        align-items: flex-start;
        gap: 10px;
      }

      .parameter-row .form-group {
        width: 120px;
        margin-bottom: 0;
      }

      .plot-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 16px;
        margin-bottom: 4px;
        flex: 0 0 auto;
      }

      .strategy-block {
        min-width: 230px;
        padding-top: 2px;
        text-align: left;
        font-family: monospace;
        font-size: 13px;
        line-height: 1.35;
      }

      .strategy-title {
        font-weight: 700;
        margin-bottom: 2px;
      }

      .right-readout {
        min-width: 430px;
        text-align: left;
      }

      .hover-readout {
        margin-top: 4px;
        font-family: monospace;
        font-size: 12px;
        line-height: 1.25;
        white-space: nowrap;
      }

      .plot-body {
        flex: 1 1 auto;
        min-height: 0;
      }
    "))
  ),
  div(
    class = "plot-half",
    div(
      class = "plot-header",
      div(
        class = "parameter-row",
        div(class = "symbol-row", textInput("symbol_top", NULL, "SPY", placeholder = "Symbol")),
        numericInput("ma20_weight", "MA20 Weight", value = 0.1, min = 0, max = 1, step = 0.05),
        numericInput("strategy_scale", "Scale", value = 7, min = 0, max = 10, step = 0.1)
      ),
      div(
        class = "right-readout",
        uiOutput("strategy_top"),
        uiOutput("hover_top")
      )
    ),
    div(
      class = "plot-body",
      plotOutput("plot_top", height = "100%", hover = hoverOpts("plot_top_hover"))
    )
  ),
  div(
    class = "plot-half",
    div(
      class = "plot-header",
      div(class = "symbol-row", textInput("symbol_bottom", NULL, "SH", placeholder = "Symbol")),
      uiOutput("hover_bottom")
    ),
    div(
      class = "plot-body",
      plotOutput("plot_bottom", height = "100%", hover = hoverOpts("plot_bottom_hover"))
    )
  )
)

server <- function(input, output, session) {
  timeframe <- "2025-04-10"

  top_data <- reactive({
    symbol <- toupper(trimws(input$symbol_top))
    if (symbol == "") return(NULL)
    get_data(symbol, timeframe)
  })

  bottom_data <- reactive({
    symbol <- toupper(trimws(input$symbol_bottom))
    if (symbol == "") return(NULL)
    get_data(symbol, timeframe)
  })

  output$plot_top <- renderPlot({
    plot_clean_price(input$symbol_top, timeframe)
  })

  output$strategy_top <- renderUI({
    strategy <- get_strategy(input$symbol_top, timeframe, input$ma20_weight, input$strategy_scale)
    validate(need(!is.null(strategy), ""))

    div(
      class = "strategy-block",
      div(class = "strategy-title", "Strategy:"),
      div(sprintf("HOLDING = %.2f", strategy$holding)),
      div(sprintf("HEDGING = %.2f", strategy$hedging)),
      div(sprintf("CASH    = %.2f", strategy$cash))
    )
  })

  output$hover_top <- renderUI({
    div(class = "hover-readout", get_hover_readout(top_data(), input$plot_top_hover))
  })

  output$hover_bottom <- renderUI({
    div(class = "hover-readout", get_hover_readout(bottom_data(), input$plot_bottom_hover))
  })

  output$plot_bottom <- renderPlot({
    plot_clean_price(input$symbol_bottom, timeframe)
  })
}

shinyApp(ui, server)


#shiny::runApp("CleanPrice_twoPlots_MA.R")
