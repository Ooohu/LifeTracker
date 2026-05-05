library(shiny)
library(shinyjs)
library(quantmod)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

DATA_FILE <- "price_records.csv"

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Stock Price Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol (e.g. AAPL)", value = "AAPL"),
      selectInput("action", "Current Action", choices = c("BUY", "SELL", "HOLD")),
      actionButton("pull", "Pull Current Price / Update Action"),
      hr(),
      actionButton("plot_symbol", "Plot Selected Symbol"),
      actionButton("plot_all", "Plot All Symbols"),
      actionButton("reset_plot", "Reset Plot"),
      hr(),
      h4("Recent Action Summary (Last 3 Months)"),
      verbatimTextOutput("recent_action_summary"),
      hr(),
      actionButton("clear", "Clear All Records", class = "btn-danger")
    ),
    
    mainPanel(
      h4("Price History"),
      DTOutput("price_table"),
      hr(),
      h4("Price Change Plot (% from Earliest BUY Price for Each Symbol)"),
      plotlyOutput("price_plot", height = "400px")
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
  plotted_symbols <- reactiveVal(character(0))
  
  observeEvent(input$pull, {
    sym <- toupper(trimws(input$symbol))
    if (sym == "") return()
    
    today <- Sys.Date()
    df    <- price_data()
    existing_idx <- which(df$Symbol == sym & df$Date == today)
    
    now <- Sys.time()
    
    tryCatch({
      if (length(existing_idx) == 0) {
        quote <- getQuote(sym)
        price <- as.numeric(quote$Last)
        
        new_row <- data.frame(
          Symbol = sym,
          Action = input$action,
          Price  = price,
          Date   = today,
          Time   = format(now, "%H:%M:%S"),
          stringsAsFactors = FALSE
        )
        
        updated <- bind_rows(new_row, df)
      } else {
        df$Action[existing_idx] <- input$action
        df$Time[existing_idx]   <- format(now, "%H:%M:%S")
        updated <- df
      }
      
      price_data(updated)
      
      current_symbols <- plotted_symbols()
      if (!(sym %in% current_symbols)) {
        plotted_symbols(c(current_symbols, sym))
      }
      
      write.csv(updated, DATA_FILE, row.names = FALSE)
      
    }, error = function(e) {
      showNotification(paste("Failed to pull price for", sym), type = "error")
    })
  })
  
  observeEvent(input$plot_symbol, {
    sym <- toupper(trimws(input$symbol))
    current_symbols <- plotted_symbols()
    if (!(sym %in% current_symbols)) {
      plotted_symbols(c(current_symbols, sym))
    }
  })
  
  observeEvent(input$plot_all, {
    all_syms <- unique(price_data()$Symbol)
    plotted_symbols(all_syms)
  })
  
  observeEvent(input$reset_plot, {
    plotted_symbols(character(0))
  })
  
  observeEvent(input$clear, {
    showModal(modalDialog(
      title = "Confirm Clear All Records",
      "Are you sure you want to delete all records? This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Yes, Clear All", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_clear, {
    removeModal()
    if (file.exists(DATA_FILE)) file.copy(DATA_FILE, paste0("backup_", Sys.Date(), ".csv"))
    price_data(empty_df)
    plotted_symbols(character(0))
    if (file.exists(DATA_FILE)) file.remove(DATA_FILE)
  })
  
  output$price_table <- renderDT({
    df <- price_data()
    if (nrow(df) == 0) return(datatable(df))
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      formatStyle('Action', target = 'cell', 
                  backgroundColor = styleEqual(c('BUY','SELL','HOLD'), c('lightgreen','lightcoral','white')))
  })
  
  output$price_plot <- renderPlotly({
    df <- price_data()
    syms <- plotted_symbols()
    if (length(syms) == 0) return(NULL)
    
    df_plot <- df %>% filter(Symbol %in% syms)
    if (nrow(df_plot) == 0) return(NULL)
    
    df_norm <- df_plot %>%
      group_by(Symbol) %>%
      filter(any(Action == "BUY")) %>%
      mutate(EarliestBuyDate = min(Date[Action == "BUY"]),
             FirstBuyPrice = Price[Date == EarliestBuyDate & Action == "BUY"],
             PctChange = 100 * (Price / FirstBuyPrice)) %>%
      ungroup()
    
    p <- ggplot(df_norm, aes(x = Date, y = PctChange, color = Symbol, group = Symbol,
                             text = paste0("Symbol: ", Symbol, "<br>Date: ", Date,
                                           "<br>Price: ", Price, "<br>Action: ", Action))) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(title = "",
           x = "Date", y = "% of Earliest BUY Price") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$recent_action_summary <- renderText({
    df <- price_data()
    if (nrow(df) == 0) return("No records available.")
    
    three_months_ago <- Sys.Date() - 90
    df_recent <- df %>% filter(Date >= three_months_ago)
    if (nrow(df_recent) == 0) return("No actions in the last 3 months.")
    
    summary_df <- df_recent %>% group_by(Symbol, Action) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(Symbol) %>%
      mutate(Fraction = n / sum(n)) %>%
      ungroup()
    
    summary_text <- summary_df %>%
      group_by(Symbol) %>%
      slice_max(Fraction) %>%
      arrange(desc(Fraction == 1 & Action == "BUY"),
              Fraction == 1 & Action == "SELL") %>%
      mutate(Text = paste0(Symbol, " - ", Action, ":", round(Fraction*100,1), "%")) %>%
      pull(Text)
    
    paste(summary_text, collapse = "\n")
  })
}

shinyApp(ui, server)

#Run with:
#shiny::runApp("Manualnputs_withPlot.R")