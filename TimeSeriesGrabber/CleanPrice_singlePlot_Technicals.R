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

sanitize_days <- function(value, default) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value) || value < 1) return(default)
  max(1, round(value))
}

sanitize_level <- function(value, default) {
  value <- suppressWarnings(as.numeric(value))
  if (is.na(value)) return(default)
  value
}

get_ema <- function(price, days) {
  EMA(price, n = days)
}

get_market_sign <- function(price, short_ema_days, long_ema_days, rsi_days) {
  short_ema <- get_ema(price, short_ema_days)
  long_ema <- get_ema(price, long_ema_days)
  macd <- MACD(price, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE)
  rsi <- RSI(price, n = rsi_days, maType = EMA)

  short_y <- as.numeric(short_ema)
  long_y <- as.numeric(long_ema)
  macd_y <- as.numeric(macd[, "macd"])
  signal_y <- as.numeric(macd[, "signal"])
  rsi_y <- as.numeric(rsi)
  market_sign <- rep(0, NROW(price))

  add_one <- function(condition) {
    !is.na(condition) & condition
  }

  market_sign <- market_sign + ifelse(add_one(short_y > long_y), 1, 0)
  market_sign <- market_sign + ifelse(add_one(macd_y > signal_y & signal_y > 0), 1, 0)
  market_sign <- market_sign + ifelse(add_one(rsi_y > 50), 1, 0)
  market_sign <- market_sign - ifelse(add_one(short_y < long_y), 1, 0)
  market_sign <- market_sign - ifelse(add_one(macd_y < signal_y & signal_y > 0), 1, 0)
  market_sign <- market_sign - ifelse(add_one(rsi_y < 50), 1, 0)

  xts(market_sign, order.by = index(price))
}

get_market_sign_color <- function(market_sign) {
  colors <- c(
    "-3" = "#7f0000",
    "-2" = "#d73027",
    "-1" = "#fcae91",
    "0" = NA,
    "1" = "#a1d99b",
    "2" = "#31a354",
    "3" = "#006d2c"
  )

  colors[as.character(pmin(pmax(round(market_sign), -3), 3))]
}

plot_clean_price <- function(symbol, from, short_ema_days, long_ema_days, rsi_days) {
  symbol <- toupper(trimws(symbol))
  validate(need(symbol != "", "Enter a symbol."))

  d <- get_data(symbol, from)
  validate(need(!is.null(d), paste("No valid price data for", symbol)))

  clean <- d$clean
  price <- d$price
  short_ema_days <- sanitize_days(short_ema_days, 15)
  long_ema_days <- sanitize_days(long_ema_days, 20)
  rsi_days <- sanitize_days(rsi_days, 15)
  ma20 <- SMA(clean, n = 20)
  ma50 <- SMA(clean, n = 50)
  short_ema <- get_ema(price, short_ema_days)
  long_ema <- get_ema(price, long_ema_days)
  market_sign <- get_market_sign(price, short_ema_days, long_ema_days, rsi_days)

  x <- index(clean)
  price_x <- index(price)
  y <- as.numeric(clean)
  ma20_y <- as.numeric(ma20)
  ma50_y <- as.numeric(ma50)
  short_ema_y <- as.numeric(short_ema)
  long_ema_y <- as.numeric(long_ema)
  y_range <- range(c(y, ma20_y, ma50_y, short_ema_y, long_ema_y), na.rm = TRUE)
  price_y <- as.numeric(price)
  price_mean <- mean(price_y, na.rm = TRUE)
  price_sd <- stats::sd(price_y, na.rm = TRUE)
  if (is.na(price_sd) || price_sd == 0) {
    price_sd <- diff(y_range) * 0.1
  }
  market_band <- c(price_mean - price_sd, price_mean + price_sd)
  y_plot_range <- range(c(y_range, market_band), na.rm = TRUE)

  par(mar = c(3.5, 4, 3, 1), xaxs = "i")
  plot(
    x,
    y,
    type = "n",
    main = paste(symbol, "Clean Price"),
    ylab = "Price",
    xlab = "",
    col = "black",
    lwd = 1.5,
    ylim = y_plot_range,
    xaxt = "n"
  )

  ticks <- pretty(price_x)
  ticks <- ticks[ticks >= min(price_x) & ticks <= max(price_x)]
  market_values <- data.frame(
    date = index(market_sign),
    market_sign = as.numeric(market_sign)
  )
  date_values <- as.numeric(market_values$date)
  day_width <- if (length(date_values) > 1) {
    stats::median(diff(date_values), na.rm = TRUE) * 0.45
  } else {
    0.45
  }

  for (i in seq_len(nrow(market_values))) {
    block_color <- get_market_sign_color(market_values$market_sign[i])
    if (!is.na(block_color)) {
      rect(
        date_values[i] - day_width,
        market_band[1],
        date_values[i] + day_width,
        market_band[2],
        col = adjustcolor(block_color, alpha.f = 0.15),
        border = NA
      )
    }
  }

  if (length(ticks) > 0) {
    axis(1, at = ticks, labels = format(ticks, "%b %Y"))
  } else {
    axis(1)
  }

  legend(
    "topright",
    legend = c("-3", "-2", "-1", "1", "2", "3"),
    fill = adjustcolor(
      c("#7f0000", "#d73027", "#fcae91", "#a1d99b", "#31a354", "#006d2c"),
      alpha.f = 0.15
    ),
    border = NA,
    title = "marketSign",
    bty = "n",
    cex = 0.75
    )

  lines(x, y, col = "black", lwd = 1.5)
  lines(x, ma20_y, col = "#1f77b4", lwd = 2)
  lines(x, ma50_y, col = "#d62728", lwd = 2)
  lines(price_x, short_ema_y, col = "#9467bd", lwd = 1.8)
  lines(price_x, long_ema_y, col = "#ff7f0e", lwd = 1.8)
  legend(
    "topleft",
    legend = c("Clean Price", "20-MA", "50-MA", "short-EMA", "long-EMA"),
    col = c("black", "#1f77b4", "#d62728", "#9467bd", "#ff7f0e"),
    lwd = c(1.5, 2, 2, 1.8, 1.8),
    bty = "n"
  )
}

plot_technicals <- function(symbol, from, short_ema_days, long_ema_days, rsi_days, overbought, oversold) {
  symbol <- toupper(trimws(symbol))
  validate(need(symbol != "", "Enter a symbol."))

  d <- get_data(symbol, from)
  validate(need(!is.null(d), paste("No valid price data for", symbol)))

  price <- d$price
  rsi_days <- sanitize_days(rsi_days, 15)
  overbought <- sanitize_level(overbought, 60)
  oversold <- sanitize_level(oversold, 40)

  macd <- MACD(price, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE)
  rsi <- RSI(price, n = rsi_days, maType = EMA)
  rsi_sma <- SMA(rsi, n = 10)

  x <- index(price)
  macd_y <- as.numeric(macd[, "macd"])
  signal_y <- as.numeric(macd[, "signal"])
  rsi_y <- as.numeric(rsi)
  rsi_sma_y <- as.numeric(rsi_sma)
  layout(matrix(c(1, 2), nrow = 2), heights = c(1, 1))

  par(mar = c(2, 4, 2.5, 1), xaxs = "i")
  macd_range <- range(c(macd_y, signal_y, 0), na.rm = TRUE)
  plot(
    x,
    macd_y,
    type = "l",
    main = paste(symbol, "MACD (12, 26, 9)"),
    ylab = "MACD",
    xlab = "",
    col = "#1f77b4",
    lwd = 1.6,
    ylim = macd_range
  )
  abline(h = 0, col = "#777777", lty = 3)
  lines(x, signal_y, col = "#d62728", lwd = 1.6)
  legend(
    "topleft",
    legend = c("MACD", "Signal"),
    col = c("#1f77b4", "#d62728"),
    lwd = 1.6,
    bty = "n"
  )

  par(mar = c(3, 4, 2.5, 1), xaxs = "i")
  plot(
    x,
    rsi_y,
    type = "l",
    main = paste(symbol, "RSI"),
    ylab = "RSI",
    xlab = "",
    col = "#2ca02c",
    lwd = 1.6,
    ylim = range(c(0, 100, rsi_y, overbought, oversold), na.rm = TRUE)
  )
  lines(x, rsi_sma_y, col = "#ff7f0e", lwd = 1.6)
  abline(h = overbought, col = "#d62728", lty = 2, lwd = 1.3)
  abline(h = oversold, col = "#1f77b4", lty = 2, lwd = 1.3)
  legend(
    "topleft",
    legend = c(
      paste0("RSI (", rsi_days, ")"),
      "RSI SMA(10)",
      paste0("OverBought ", overbought),
      paste0("OverSold ", oversold)
    ),
    col = c("#2ca02c", "#ff7f0e", "#d62728", "#1f77b4"),
    lwd = c(1.6, 1.6, 1.3, 1.3),
    lty = c(1, 1, 2, 2),
    bty = "n"
  )
}

clip01 <- function(x) {
  pmin(pmax(x, 0), 1)
}

get_hover_readout <- function(data, hover, short_ema_days, long_ema_days, rsi_days, include_technicals = FALSE) {
  empty_hover_readout <- function() {
    div(
      div(class = "hover-values", "Hover plot for values"),
      div(class = "hover-signals", "")
    )
  }

  if (is.null(data) || is.null(hover)) return(empty_hover_readout())

  clean <- data$clean
  price <- data$price
  short_ema_days <- sanitize_days(short_ema_days, 15)
  long_ema_days <- sanitize_days(long_ema_days, 20)
  rsi_days <- sanitize_days(rsi_days, 15)
  short_ema <- get_ema(price, short_ema_days)
  long_ema <- get_ema(price, long_ema_days)
  macd <- MACD(price, nFast = 12, nSlow = 26, nSig = 9, maType = EMA, percent = FALSE)
  rsi <- RSI(price, n = rsi_days, maType = EMA)
  market_sign <- get_market_sign(price, short_ema_days, long_ema_days, rsi_days)
  values <- data.frame(
    date = index(price),
    close = as.numeric(price),
    short_ema = as.numeric(short_ema),
    long_ema = as.numeric(long_ema),
    macd = as.numeric(macd[, "macd"]),
    signal = as.numeric(macd[, "signal"]),
    rsi = as.numeric(rsi),
    market_sign = as.numeric(market_sign)
  )
  clean_values <- data.frame(
    date = index(clean),
    price = as.numeric(clean),
    ma20 = as.numeric(SMA(clean, n = 20)),
    ma50 = as.numeric(SMA(clean, n = 50))
  )

  if (nrow(values) < 1 || is.null(hover$x)) return(empty_hover_readout())

  i <- which.min(abs(as.numeric(values$date) - as.numeric(hover$x)))
  clean_i <- which.min(abs(as.numeric(clean_values$date) - as.numeric(hover$x)))
  row <- values[i, ]
  clean_row <- clean_values[clean_i, ]
  format_value <- function(value) {
    ifelse(is.na(value), "NA", sprintf("%.2f", value))
  }
  signal_flag <- function(condition) {
    isTRUE(!is.na(condition) && condition)
  }
  signal_span <- function(label, value) {
    span(
      class = ifelse(value, "signal-true", "signal-false"),
      sprintf("%s %s", label, ifelse(value, "TRUE", "FALSE"))
    )
  }
  ema_signal <- signal_flag(row$short_ema > row$long_ema)
  macd_signal <- signal_flag(row$macd > row$signal && row$signal > 0)
  rsi_signal <- signal_flag(row$rsi > 50)
  technical_text <- if (include_technicals) {
    sprintf(
      " | MACD %s | Signal %s | RSI(%d) %s",
      format_value(row$macd),
      format_value(row$signal),
      rsi_days,
      format_value(row$rsi)
    )
  } else {
    ""
  }

  div(
    div(
      class = "hover-values",
      sprintf(
        "%s | P %s | 20-MA %s | 50-MA %s | short-EMA(%d) %s | long-EMA(%d) %s%s | marketSign %d",
        format(row$date, "%Y-%m-%d"),
        format_value(row$close),
        format_value(clean_row$ma20),
        format_value(clean_row$ma50),
        short_ema_days,
        format_value(row$short_ema),
        long_ema_days,
        format_value(row$long_ema),
        technical_text,
        row$market_sign
      )
    ),
    div(
      class = "hover-signals",
      signal_span("short-EMA > long-EMA:", ema_signal),
      span(" | "),
      signal_span("MACD > Signal > 0:", macd_signal),
      span(" | "),
      signal_span("RSI > 50:", rsi_signal)
    )
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

  list(
    holding = holding,
    hedging = hedging
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
        height: calc((100vh - 44px) / 2);
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
        min-width: 230px;
        text-align: left;
      }

      .hover-strip {
        height: 44px;
        display: flex;
        align-items: center;
        justify-content: center;
        border-bottom: 1px solid #ddd;
        box-sizing: border-box;
        padding: 0 14px;
      }

      .hover-readout {
        font-family: monospace;
        font-size: 12px;
        line-height: 1.3;
        text-align: center;
        width: 100%;
      }

      .hover-values,
      .hover-signals {
        white-space: nowrap;
      }

      .signal-true {
        color: #16833a;
        font-weight: 700;
      }

      .signal-false {
        color: #c62828;
        font-weight: 700;
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
        numericInput("short_ema_days", "short-EMA Days", value = 6, min = 1, step = 1, width = "95px"),
        numericInput("long_ema_days", "long-EMA Days", value = 26, min = 1, step = 1, width = "95px"),
        numericInput("ma20_weight", "MA20 Weight", value = 0.25, min = 0, max = 1, step = 0.05),
        numericInput("strategy_scale", "Scale", value = 1.7, min = 0, max = 10, step = 0.1)
      ),
      div(
        class = "right-readout",
        uiOutput("strategy_top")
      )
    ),
    div(
      class = "plot-body",
      plotOutput(
        "plot_top",
        height = "100%",
        hover = hoverOpts("plot_top_hover", nullOutside = FALSE)
      )
    )
  ),
  div(
    class = "hover-strip",
    uiOutput("hover_readout", class = "hover-readout")
  ),
  div(
    class = "plot-half",
    div(
      class = "plot-header",
      div(
        class = "parameter-row",
        numericInput("rsi_days", "RSI Days", value = 13, min = 1, step = 1),
        numericInput("overbought", "OverBought", value = 60, min = 0, max = 100, step = 1),
        numericInput("oversold", "OverSold", value = 40, min = 0, max = 100, step = 1)
      )
    ),
    div(
      class = "plot-body",
      plotOutput(
        "plot_bottom",
        height = "100%",
        hover = hoverOpts("plot_bottom_hover", nullOutside = FALSE)
      )
    )
  )
)

server <- function(input, output, session) {
  timeframe <- "2025-04-10"
  last_hover_plot <- reactiveVal("top")
  last_hover <- reactiveVal(NULL)

  top_data <- reactive({
    symbol <- toupper(trimws(input$symbol_top))
    if (symbol == "") return(NULL)
    get_data(symbol, timeframe)
  })

  output$plot_top <- renderPlot({
    plot_clean_price(
      input$symbol_top,
      timeframe,
      input$short_ema_days,
      input$long_ema_days,
      input$rsi_days
    )
  })

  output$strategy_top <- renderUI({
    strategy <- get_strategy(input$symbol_top, timeframe, input$ma20_weight, input$strategy_scale)
    validate(need(!is.null(strategy), ""))

    div(
      class = "strategy-block",
      div(class = "strategy-title", "Strategy:"),
      div(sprintf("HOLDING = %.2f", strategy$holding)),
      div(sprintf("HEDGING = %.2f", strategy$hedging))
    )
  })

  observeEvent(input$plot_top_hover, {
    last_hover_plot("top")
    last_hover(input$plot_top_hover)
  })

  observeEvent(input$plot_bottom_hover, {
    last_hover_plot("bottom")
    last_hover(input$plot_bottom_hover)
  })

  output$hover_readout <- renderUI({
    get_hover_readout(
      top_data(),
      last_hover(),
      input$short_ema_days,
      input$long_ema_days,
      input$rsi_days,
      include_technicals = last_hover_plot() == "bottom"
    )
  })

  output$plot_bottom <- renderPlot({
    plot_technicals(
      input$symbol_top,
      timeframe,
      input$short_ema_days,
      input$long_ema_days,
      input$rsi_days,
      input$overbought,
      input$oversold
    )
  })
}

shinyApp(ui, server)


#shiny::runApp("CleanPrice_singlePlot_Technicals.R")
