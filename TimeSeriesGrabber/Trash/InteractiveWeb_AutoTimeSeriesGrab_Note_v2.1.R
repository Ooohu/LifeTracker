library(shiny)
library(quantmod)
library(dplyr)
library(fitdistrplus)
library(DT)

# -----------------------------
# Helpers
# -----------------------------

get_data <- function(symbol, from) {
  
  data <- tryCatch(
    getSymbols(symbol, auto.assign = FALSE, from = from),
    error = function(e) NULL
  )
  
  if (is.null(data) || NROW(data) < 5) return(NULL)
  
  price <- tryCatch(Cl(data), error = function(e) NULL)
  if (is.null(price) || NROW(price) < 5) return(NULL)
  
  peaks <- tryCatch(
    price[price > lag(price,1) & price > lead(price,1)],
    error = function(e) NULL
  )
  
  troughs <- tryCatch(
    price[price < lag(price,1) & price < lead(price,1)],
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
  
  list(
    price = price,
    clean = clean
  )
}

get_fluc_fit <- function(price, trim = 0.05) {
  
  if (is.null(price) || NROW(price) < 10) return(NULL)
  
  price_vec <- as.numeric(price)
  
  min_i <- which.min(price_vec)
  if (is.na(min_i) || min_i >= length(price_vec)) return(NULL)
  
  after <- price_vec[(min_i + 1):length(price_vec)]
  if (length(after) < 2) return(NULL)
  
  peak_i <- min_i + which.max(after)
  if (is.na(peak_i) || peak_i <= min_i) return(NULL)
  
  seg <- price_vec[min_i:peak_i]
  if (length(seg) < 3) return(NULL)
  
  pct <- 100 * diff(seg) / head(seg, -1)
  pct <- na.omit(pct)
  
  if (length(pct) < 5) return(NULL)
  
  #lo <- quantile(pct, trim)
  hi <- quantile(pct, 1 - trim)
  
  #pct_trim <- pct[pct >= lo & pct <= hi]
  pct_trim <- pct[pct <= hi]
  if (length(pct_trim) < 5) return(NULL)
  
  fit <- tryCatch(
    fitdist(pct_trim, "norm"),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  list(
    fluc = pct,
    fluc_trim = pct_trim,
    mu = fit$estimate["mean"],
    sigma = fit$estimate["sd"]
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  titlePanel("Multi-Ticker Risk Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Ticker", "GOOGL"),
      actionButton("go", "Run")
    ),
    
    mainPanel(
      DTOutput("table"),
      plotOutput("plot_price", height = "160px"),
      plotOutput("plot_dist", height = "240px")
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  
  file_path <- "InquiryRecord.csv"
  timeframe <- "2025-04-10"
  
  rv <- reactiveValues(
    results = if (file.exists(file_path)) read.csv(file_path)
    else data.frame(
      ID = integer(),
      Symbol=character(), VaR_05=numeric(),
      BigDrop=numeric(), Frac1=numeric(),
      Frac2=numeric(), Risk=numeric(),
      LastMod = character()
    ),
    selected = NULL
  )
  
  
  # -------------------------
  # Run analysis
  # -------------------------
  
  observeEvent(input$go, {
    
    sym <- toupper(trimws(input$symbol))
    
    d <- get_data(sym, timeframe)
    
    if (is.null(d)) {
      showNotification("No valid price data", type = "error")
      return()
    }
    
    f <- get_fluc_fit(d$price)
    
    if (is.null(f)) {
      showNotification("Insufficient data for fitting. This ticker is basically a collapsing waterfall with no statistical dignity left.", type = "error")
      return()
    }
    
    VaR <- qnorm(0.05, f$mu, f$sigma)
    worst <- min(f$fluc)
    
    f1 <- 0.5
    f2 <- 1 - f1
    risk <- f1 * VaR + f2 * worst
    
    idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
    
    if (!"ID" %in% colnames(rv$results)) {
      rv$results$ID <- seq_len(nrow(rv$results))
    }
    
    new_id <- if (nrow(rv$results) == 0) {
      1
    } else {
      max(rv$results$ID, na.rm = TRUE) + 1
    }
    
    new_row <- data.frame(
      ID = new_id,
      Symbol = sym,
      VaR_05 = VaR,
      BigDrop = worst,
      Frac1 = f1,
      Frac2 = f2,
      Risk = risk,
      LastMod = format(Sys.time(), "%Y-%m-%d")
    )
    
    if (length(idx) == 0) {
      rv$results <- rbind(rv$results, new_row)
    } else {
      new_row$ID <- rv$results$ID[idx]  # preserve ID
      rv$results[idx, ] <- new_row[1, colnames(rv$results)]
    }
    
    rv$results$Symbol <- toupper(trimws(rv$results$Symbol))
    write.csv(rv$results, file_path, row.names = FALSE)
    
    rv$selected <- list(
      fluc = f$fluc,
      mu = f$mu,
      sigma = f$sigma,
      symbol = sym
    )
  })
  
  # -------------------------
  # Table (Frac1 editable only)
  # -------------------------
  
  output$table <- renderDT({
    datatable(
      selection = "single",
      rv$results,
      editable = list(
        target = "cell",
        disable = list(columns = c(0,1,2,4,5,6))  # only Frac1 editable (index 3)
      ),
      rownames = FALSE,
      escape = FALSE,
      options = list(scrollX = TRUE)
      
    )%>%
      formatRound(
        columns = c("VaR_05", "BigDrop", "Frac1", "Frac2", "Risk"),
        digits = 2
      )
  })
  
  # -------------------------
  # Edit handler
  # -------------------------
  
  observeEvent(input$table_cell_edit, {
    
    e <- input$table_cell_edit
    i <- e$row
    j <- e$col
    
    if (j!=3) return()
    
    v <- as.numeric(e$value)
    if (is.na(v)) return()
    
    v <- max(0, min(1, round(v / 0.05) * 0.05))
    
    rv$results$Frac1[i] <- v
    rv$results$Frac2[i] <- 1 - v
    
    rv$results$Risk[i] <-
      v * rv$results$VaR_05[i] +
      (1 - v) * rv$results$BigDrop[i]
    
    rv$results$LastMod[i] <- format(Sys.time(), "%Y-%m-%d")
    
    write.csv(rv$results, file_path, row.names = FALSE)
  })
  
  observeEvent(input$table_rows_selected, {
    
    i <- input$table_rows_selected
    req(length(i) == 1)
    req(i > 0)
    req(i <= nrow(rv$results))
    
    sym <- toupper(trimws(rv$results$Symbol[[i]]))
    req(!is.null(sym), !is.na(sym), sym != "")
    
    # enforce capitalization back into the table (keeps UI consistent)
    rv$results$Symbol[[i]] <- sym
    
    d <- get_data(sym, timeframe)
    req(!is.null(d))
    
    f <- get_fluc_fit(d$price)
    req(!is.null(f))
    
    rv$selected <- list(
      fluc = f$fluc,
      mu = f$mu,
      sigma = f$sigma,
      symbol = sym
    )
  })
  # -------------------------
  # Plot
  # -------------------------
  output$plot_price <- renderPlot({
    
    req(rv$selected)
    
    d <- get_data(rv$selected$symbol, timeframe)
    
    plot(d$price,
         main = paste0(rv$selected$symbol, " Clean Price"),
         ylab = "Price",
         col = "black")
  })
  
  output$plot_dist <- renderPlot({
    
    req(rv$selected)
    
    hist(rv$selected$fluc,
         breaks = 50,
         probability = TRUE,
         col = "lightblue",
         border = "white",
         main = paste0(rv$selected$symbol, " Distribution from: ", timeframe),
         xlab = "% Change")
    
    curve(dnorm(x, rv$selected$mu, rv$selected$sigma),
          add = TRUE, col = "red", lwd = 2)
  })
  
}# End of server

shinyApp(ui, server)


#shiny::runApp("InteractiveWeb_AutoTimeSeriesGrab_Note_v2.1.R")