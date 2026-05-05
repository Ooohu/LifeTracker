library(shiny)
library(quantmod)
library(dplyr)
library(fitdistrplus)

# -----------------------------
# Functions (your pipeline)
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
  
  titlePanel("Multi-Ticker Stock Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Ticker Symbol", value = "GOOGL"),
      actionButton("go", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Price",
                 plotOutput("pricePlot")),
        
        tabPanel("Peaks/Troughs",
                 plotOutput("cleanPlot")),
        
        tabPanel("Big Drop",
                 tableOutput("dropTable")),
        
        tabPanel("Fluctuations",
                 plotOutput("flucHist"),
                 verbatimTextOutput("stats")),
        
        tabPanel("Return Distribution",
                 plotOutput("retHist"))
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output) {
  
  data_reactive <- eventReactive(input$go, {
    
    getSymbols(input$symbol, auto.assign = FALSE, from = "2026-01-01")
  })
  
  clean_reactive <- reactive({
    
    data <- data_reactive()
    
    peaks <- find_peaks(data)
    troughs <- find_troughs(data)
    
    clean <- rbind(peaks, troughs)
    clean[order(index(clean))]
  })
  
  output$pricePlot <- renderPlot({
    data <- data_reactive()
    plot(Cl(data), main = paste(input$symbol, "Price"))
  })
  
  output$cleanPlot <- renderPlot({
    clean <- clean_reactive()
    plot(Cl(clean), type = "b", main = "Clean Price (Peaks + Troughs)")
  })
  
  output$dropTable <- renderTable({
    clean <- clean_reactive()
    biggest_drop(clean)
  })
  
  output$flucHist <- renderPlot({
    
    clean <- clean_reactive()
    seg <- bull_segment(clean)
    
    fluc <- get_fluctuations(
      Cl(clean),
      seg$min_date,
      seg$peak_date
    )
    
    hist(fluc,
         breaks = 30,
         col = "lightblue",
         main = "Bull Segment Fluctuations")
  })
  
  output$stats <- renderPrint({
    
    clean <- clean_reactive()
    seg <- bull_segment(clean)
    
    fluc <- get_fluctuations(
      Cl(clean),
      seg$min_date,
      seg$peak_date
    )
    
    cat("Min:", min(fluc), "\n")
    cat("Mean:", mean(fluc), "\n")
    cat("SD:", sd(fluc), "\n")
  })
  
  output$retHist <- renderPlot({
    
    clean <- clean_reactive()
    
    ret <- diff(Cl(clean)) / lag(Cl(clean)) * 100
    ret <- na.omit(ret)
    
    hist(ret,
         breaks = 50,
         col = "lightblue",
         main = "Return Distribution")
  })
}

# -----------------------------
# Run app
# -----------------------------

shinyApp(ui, server)
