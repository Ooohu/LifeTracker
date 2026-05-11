library(shiny)
library(quantmod)
library(dplyr)
library(fitdistrplus)
library(DT)

# -----------------------------
# Functions
# -----------------------------

find_peaks <- function(data) {
  price <- Cl(data)
  prev  <- lag(price, 1)
  nexts <- lead(price, 1)
  peaks <- (price > prev) & (price > nexts)
  peaks[is.na(peaks)] <- FALSE
  data[peaks]
}

find_troughs <- function(data) {
  price <- Cl(data)
  prev  <- lag(price, 1)
  nexts <- lead(price, 1)
  trough <- (price < prev) & (price < nexts)
  trough[is.na(trough)] <- FALSE
  data[trough]
}

biggest_drop <- function(clean_price) {
  price <- Cl(clean_price)
  
  run_peak <- cummax(price)
  dd <- price - run_peak
  
  end_i <- which.min(dd)
  start_i <- which.max(price[1:end_i])
  
  start <- as.numeric(price[start_i])
  end   <- as.numeric(price[end_i])
  
  data.frame(
    start_date = index(price)[start_i],
    end_date   = index(price)[end_i],
    drop_pct   = (start - end) / start * 100
  )
}

get_fluctuations <- function(price, min_date, peak_date) {
  segment <- price[paste(min_date, peak_date, sep = "/")]
  prev <- lag(segment, 1)
  
  drop_pct <- (as.numeric(segment) - as.numeric(prev)) /
    as.numeric(prev) * 100
  
  drop_pct[!is.na(drop_pct)]
}

bull_segment <- function(clean_price) {
  price <- Cl(clean_price)
  
  min_i <- which.min(price)
  after_min <- price[(min_i + 1):NROW(price)]
  
  peak_i <- min_i + which.max(after_min)
  
  list(
    min_date = index(price)[min_i],
    peak_date = index(price)[peak_i]
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Multi-Ticker Risk Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Ticker Symbol", value = "GOOGL"),
      numericInput("frac1", "Frac1 (0-1)", value = 0.5, min = 0, max = 1),
      actionButton("go", "Run Analysis")
    ),
    
    mainPanel(
      DTOutput("resultsTable"),
      plotOutput("gaussPlot")
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  file_path <- "InquiryRecord.csv"
  time_frame <- "2025-04-10"
  
  # -------------------------
  # Load previous results
  # -------------------------
  
  if (file.exists(file_path)) {
    rv$results <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    rv$results <- data.frame(
      Symbol = character(),
      VaR_05 = numeric(),
      Worst_drop = numeric(),
      Frac1 = numeric(),
      Frac2 = numeric(),
      Risk = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  selected_data <- reactiveVal(NULL)
  
  # -------------------------
  # MAIN ANALYSIS
  # -------------------------
  
  observeEvent(input$go, {
    
    symbol <- input$symbol
    
    # remove existing row (overwrite behavior)
    rv$results <- rv$results[rv$results$Symbol != symbol, ]
    
    data <- getSymbols(symbol,
                       auto.assign = FALSE,
                       from = time_frame)
    
    peaks <- find_peaks(data)
    troughs <- find_troughs(data)
    
    clean_price <- rbind(peaks, troughs)
    clean_price <- clean_price[order(index(clean_price))]
    
    BullPeriod <- bull_segment(clean_price)
    
    price <- Cl(clean_price)
    
    fluc_pct <- get_fluctuations(
      price,
      BullPeriod$min_date,
      BullPeriod$peak_date
    )
    
    fluc_pct <- as.numeric(fluc_pct)
    fluc_pct <- fluc_pct[!is.na(fluc_pct)]
    
    # Gaussian fit
    fit <- fitdist(fluc_pct, "norm")
    
    mu <- fit$estimate["mean"]
    sigma <- fit$estimate["sd"]
    
    VaR_05 <- qnorm(0.05, mean = mu, sd = sigma)
    worst_drop <- min(fluc_pct)
    
    frac1 <- input$frac1
    frac2 <- 1 - frac1
    
    risk_score <- frac1 * VaR_05 + frac2 * worst_drop
    
    # append table
    rv$results <- rbind(
      rv$results,
      data.frame(
        Symbol = symbol,
        VaR_05 = signif(VaR_05, 2),
        Worst_drop = signif(worst_drop, 2),
        Frac1 = signif(frac1, 2),
        Frac2 = signif(frac2, 2),
        Risk = signif(risk_score, 2),
        stringsAsFactors = FALSE
      )
    )
    
    write.csv(rv$results, file_path, row.names = FALSE)
    
    selected_data(list(
      symbol = symbol,
      fluc = fluc_pct,
      mu = mu,
      sigma = sigma
    ))
  })
  
  # -------------------------
  # TABLE (sortable DT)
  # -------------------------
  
  output$resultsTable <- renderDT({
    
    datatable(
      rv$results,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # -------------------------
  # CLICK ROW → UPDATE PLOT
  # -------------------------
  
  observeEvent(input$resultsTable_rows_selected, {
    
    req(rv$results)
    
    i <- input$resultsTable_rows_selected
    symbol <- rv$results$Symbol[i]
    
    data <- getSymbols(symbol,
                       auto.assign = FALSE,
                       from = time_frame)
    
    peaks <- find_peaks(data)
    troughs <- find_troughs(data)
    
    clean_price <- rbind(peaks, troughs)
    clean_price <- clean_price[order(index(clean_price))]
    
    BullPeriod <- bull_segment(clean_price)
    
    price <- Cl(clean_price)
    
    fluc_pct <- get_fluctuations(
      price,
      BullPeriod$min_date,
      BullPeriod$peak_date
    )
    
    fluc_pct <- as.numeric(fluc_pct)
    fluc_pct <- fluc_pct[!is.na(fluc_pct)]
    
    fit <- fitdist(fluc_pct, "norm")
    
    selected_data(list(
      symbol = symbol,
      fluc = fluc_pct,
      mu = fit$estimate["mean"],
      sigma = fit$estimate["sd"]
    ))
  })
  
  # -------------------------
  # PLOT
  # -------------------------
  
  output$gaussPlot <- renderPlot({
    
    req(selected_data())
    
    d <- selected_data()
    
    hist(d$fluc,
         breaks = 50,
         probability = TRUE,
         main = paste0("Distribution (from ", time_frame, ")"),
         xlab = "% Change",
         col = "lightblue")
    
    curve(dnorm(x, mean = d$mu, sd = d$sigma),
          add = TRUE,
          col = "red",
          lwd = 2)
  })
}

# -----------------------------
# RUN APP
# -----------------------------

shinyApp(ui, server)