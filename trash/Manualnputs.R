# =============================
# Stock Price Tracker (RStudio / Shiny Template)
# =============================

library(shiny)
library(quantmod)
library(DT)
library(dplyr)

DATA_FILE <- "price_records.csv"

ui <- fluidPage(
  titlePanel("Stock Price Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol (e.g. AAPL)", value = "AAPL"),
      selectInput("action", "Action", choices = c("BUY", "SELL")),
      actionButton("pull", "Pull Current Price"),
      hr(),
      actionButton("clear", "Clear All Records")
    ),
    
    mainPanel(
      h4("Transaction History"),
      DTOutput("price_table")
    )
  )
)

server <- function(input, output, session) {
  
  empty_df <- data.frame(
    Symbol = character(0),
    Action = character(0),
    Price  = numeric(0),
    Date   = as.Date(character(0)),
    Time   = character(0),
    stringsAsFactors = FALSE
  )
  
  if (file.exists(DATA_FILE)) {
    initial_df <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
    initial_df$Date <- as.Date(initial_df$Date)
  } else {
    initial_df <- empty_df
  }
  
  price_data <- reactiveVal(initial_df)
  
  observeEvent(input$pull, {
    sym <- toupper(trimws(input$symbol))
    if (sym == "") return()
    
    tryCatch({
      quote <- getQuote(sym)
      price <- as.numeric(quote$Last)
      now   <- Sys.time()
      
      new_row <- data.frame(
        Symbol = sym,
        Action = input$action,
        Price  = price,
        Date   = as.Date(now),
        Time   = format(now, "%H:%M:%S"),
        stringsAsFactors = FALSE
      )
      
      updated <- bind_rows(price_data(), new_row)
      price_data(updated)
      write.csv(updated, DATA_FILE, row.names = FALSE)
      
    }, error = function(e) {
      showNotification("Failed to pull price. Check symbol.", type = "error")
    })
  })
  
  observeEvent(input$clear, {
    price_data(empty_df)
    if (file.exists(DATA_FILE)) file.remove(DATA_FILE)
  })
  
  output$price_table <- renderDT({
    datatable(
      price_data(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
