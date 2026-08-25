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

biggest_drop <- function(clean_price) {
  
  if (is.null(clean_price) || NROW(clean_price) < 2) return(NULL)
  
  price <- tryCatch(Cl(clean_price), error = function(e) clean_price)
  price <- na.omit(price)
  
  if (is.null(price) || NROW(price) < 2) return(NULL)
  
  price_vec <- as.numeric(price)
  run_peak <- cummax(price_vec)
  dd <- 100 * (price_vec - run_peak) / run_peak
  
  end_i <- which.min(dd)
  if (is.na(end_i) || end_i <= 1) return(0)
  
  start_i <- which.max(price_vec[1:end_i])
  
  start <- price_vec[start_i]
  end <- price_vec[end_i]
  
  drop <- (end - start) / start * 100
  as.numeric(drop)
}

normalize_results <- function(results) {
  
  cols <- c("Bank", "Symbol", "IWgt", "FWgt", "RefP", "CurP", "VaR_05", "BigDrop", "ADR5", "LastMod")
  
  if (is.null(results) || nrow(results) == 0) {
    return(data.frame(
      Bank = character(), Symbol = character(), IWgt = numeric(), FWgt = numeric(),
      RefP = numeric(), CurP = numeric(),
      VaR_05 = numeric(), BigDrop = numeric(), ADR5 = numeric(),
      LastMod = character()
    ))
  }
  
  results <- as.data.frame(results, stringsAsFactors = FALSE)
  
  for (col in setdiff(cols, names(results))) {
    if (col == "Bank") {
      results[[col]] <- rep("zNA", nrow(results))
    } else if (col %in% c("Symbol", "LastMod")) {
      results[[col]] <- rep("", nrow(results))
    } else if (col == "RefP") {
      results[[col]] <- rep(NA_real_, nrow(results))
    } else {
      results[[col]] <- rep(0, nrow(results))
    }
  }
  
  results <- results[, cols, drop = FALSE]
  results$Bank <- trimws(as.character(results$Bank))
  results$Bank[is.na(results$Bank) | !results$Bank %in% c("Tier1", "Tier2","Tier3", "zNA")] <- "zNA"
  results$IWgt <- as.numeric(results$IWgt)
  results$IWgt[is.na(results$IWgt)] <- 0
  results$Symbol <- toupper(trimws(results$Symbol))
  
  numeric_cols <- c("IWgt", "FWgt", "RefP", "CurP", "VaR_05", "BigDrop", "ADR5")
  results[numeric_cols] <- lapply(results[numeric_cols], as.numeric)
  results$RefP <- round(results$RefP, 1)
  results$LastMod <- as.character(results$LastMod)
  
  rownames(results) <- NULL
  results
}

table_row_data <- function(results, i) {
  row <- normalize_results(results)[i, , drop = FALSE]
  unname(as.list(row[1, ]))
}


get_fear_gauge <- function() {
  vix <- tryCatch(
    getSymbols("^VIX", auto.assign = FALSE, from = Sys.Date() - 60),
    error = function(e) NULL
  )

  if (is.null(vix) || NROW(vix) == 0) {
    return(NULL)
  }

  close <- tryCatch(na.omit(Cl(vix)), error = function(e) NULL)
  if (is.null(close) || NROW(close) == 0) {
    return(NULL)
  }

  close_values <- as.numeric(close)
  if (length(close_values) < 20) {
    return(NULL)
  }

  list(
    current = tail(close_values, 1),
    sma_20 = mean(tail(close_values, 20)),
    sma_10 = mean(tail(close_values, 10))
  )
}
# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  tags$style(HTML("
    #table input,
    #table select,
    #table textarea {
      color: #111111 !important;
      background-color: #ffffff !important;
    }

    #table .IWgt-input {
      width: 3.5em !important;
      padding: 2px !important;
      box-sizing: border-box;
    }

    #table table.dataTable th:nth-child(2),
    #table table.dataTable td:nth-child(2) {
      width: 6ch !important;
      min-width: 6ch !important;
      max-width: 6ch !important;
    }

    #table table.dataTable th:nth-child(3),
    #table table.dataTable td:nth-child(3) {
      width: 56px !important;
      min-width: 56px !important;
      max-width: 56px !important;
    }

    #table tbody tr.refp-above-curp > td,
    #table tbody tr.refp-above-curp input,
    #table tbody tr.refp-above-curp select {
      background-color: #fff1f1 !important;
    }

    #table tbody tr.refp-below-curp > td,
    #table tbody tr.refp-below-curp input,
    #table tbody tr.refp-below-curp select {
      background-color: #f1fbf3 !important;
    }

    .title-row {
      display: flex;
      align-items: baseline;
      gap: 16px;
      flex-wrap: wrap;
      margin-bottom: 12px;
    }

    .title-row h2 {
      margin: 0;
    }

    .fear-gauge {
      font-size: 15px;
      font-weight: 600;
      color: #333333;
      white-space: nowrap;
    }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateRiskTableRow', function(message) {
      var tableNode = $('#table table.dataTable');
      if (!tableNode.length || !$.fn.dataTable.isDataTable(tableNode)) return;

      var table = tableNode.DataTable();
      var symbol = String(message.symbol || '').toUpperCase();
      var updated = false;

      table.rows().every(function() {
        var rowData = this.data();
        if (String(rowData[1] || '').toUpperCase() === symbol) {
          this.data(message.rowData);
          updated = true;
        }
      });

      if (!updated) {
        table.row.add(message.rowData);
      }

      table.draw(false);
    });

    function getRiskTableRowData(element) {
      var tableNode = $('#table table.dataTable');
      if (!tableNode.length || !$.fn.dataTable.isDataTable(tableNode)) return null;

      var table = tableNode.DataTable();
      var row = table.row($(element).closest('tr'));
      if (!row.length) return null;

      return { table: table, row: row, data: row.data() };
    }

    $(document).on('change', '#table select.bank-select', function() {
      var row = getRiskTableRowData(this);
      if (!row || !row.data) return;

      row.data[0] = this.value;
      row.row.data(row.data).draw(false);
      Shiny.setInputValue('table_bank_change', {
        symbol: row.data[1],
        value: this.value,
        nonce: Math.random()
      }, {priority: 'event'});
    });

    $(document).on('change', '#table input.IWgt-input', function() {
      var row = getRiskTableRowData(this);
      if (!row || !row.data) return;

      var value = Number(this.value);
      if (!isFinite(value)) value = 0;

      row.data[2] = value;
      row.row.data(row.data).draw(false);
      Shiny.setInputValue('table_iwgt_change', {
        symbol: row.data[1],
        value: value,
        nonce: Math.random()
      }, {priority: 'event'});
    });

    $(document).on('change', '#table input.refp-input', function() {
      var row = getRiskTableRowData(this);
      if (!row || !row.data) return;

      var value = this.value === '' ? null : Number(this.value);
      if (value !== null && !isFinite(value)) return;

      row.data[4] = value;
      row.row.data(row.data).draw(false);
      Shiny.setInputValue('table_refp_change', {
        symbol: row.data[1],
        value: value,
        nonce: Math.random()
      }, {priority: 'event'});
    });

    $(document).on('focusin', '#table input[type=number]', function() {
      if ($(this).hasClass('IWgt-input') || $(this).hasClass('refp-input')) {
        this.step = 'any';
        this.removeAttribute('min');
        this.removeAttribute('max');
      } else {
        this.step = '0.1';
        this.min = '0';
        this.max = '1';
      }
    });
  ")),
  tags$div(
    class = "title-row",
    tags$h2("Multi-Ticker Risk Dashboard"),
    tags$span(class = "fear-gauge", "Fear Gauge (VIX): ", textOutput("fear_gauge", inline = TRUE))
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Ticker", "GOOGL"),
      actionButton("go", "Run"),
      actionButton("scan_all", "Scan All")
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
    results = normalize_results(if (file.exists(file_path)) read.csv(file_path) else NULL),
    selected = NULL
  )

  output$fear_gauge <- renderText({
    vix <- get_fear_gauge()
    if (is.null(vix) || any(is.na(unlist(vix)))) {
      return("--")
    }

    paste0(
      format(round(vix$current, 2), nsmall = 2),
      " | 10-SMA: ", format(round(vix$sma_10, 2), nsmall = 2),
      " | 20-SMA: ", format(round(vix$sma_20, 2), nsmall = 2)
    )
  })

  get_table_row <- function(display_row) {
    if (length(display_row) != 1 || is.na(display_row)) {
      return(display_row)
    }

    if (!is.null(input$table_rows_current) &&
        length(input$table_rows_current) >= display_row) {
      input$table_rows_current[[display_row]]
    } else {
      display_row
    }
  }
  
  # -------------------------
  # Run analysis
  # -------------------------

  refresh_ticker <- function(symbol, set_selected = TRUE) {
    sym <- toupper(trimws(symbol))
    d <- get_data(sym, timeframe)

    if (is.null(d)) {
      return(list(ok = FALSE, message = paste("No valid price data for", sym)))
    }

    close_values <- as.numeric(na.omit(d$price))
    if (length(close_values) < 6) {
      return(list(ok = FALSE, message = paste("Insufficient closing-price data for", sym)))
    }

    f <- get_fluc_fit(d$price)
    if (is.null(f)) {
      return(list(ok = FALSE, message = paste("Insufficient data for fitting", sym)))
    }

    worst <- biggest_drop(d$price)
    if (is.null(worst)) {
      return(list(ok = FALSE, message = paste("Insufficient data for biggest drop calculation", sym)))
    }

    VaR <- qnorm(0.05, f$mu, f$sigma)

    rv$results <- normalize_results(rv$results)
    idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
    bank <- if (length(idx) > 0) rv$results$Bank[[idx[[1]]]] else "zNA"
    iwgt <- if (length(idx) > 0) as.numeric(rv$results$IWgt[[idx[[1]]]]) else 0
    refp <- if (length(idx) > 0) as.numeric(rv$results$RefP[[idx[[1]]]]) else NA_real_

    if (is.na(bank) || !bank %in% c("Tier1", "Tier2", "Tier3", "zNA")) {
      bank <- "zNA"
    }
    if (is.na(iwgt)) {
      iwgt <- 0
    }
    if (is.na(refp)) {
      refp <- min(tail(close_values, 5))
    }
    refp <- round(refp, 1)

    curp <- tail(close_values, 1)
    adr5 <- 100*mean(tail(diff(close_values) / head(close_values, -1), 5))
    high_52wk <- max(tail(close_values, 252))
    fwgt <- if (refp == 0 || worst == 0) NA_real_ else iwgt * (high_52wk / refp) / abs(worst * 0.01)

    new_row <- data.frame(
      Bank = bank,
      Symbol = sym,
      IWgt = iwgt,
      FWgt = fwgt,
      RefP = refp,
      CurP = curp,
      VaR_05 = VaR,
      BigDrop = worst,
      ADR5 = adr5,
      LastMod = format(Sys.time(), "%Y-%m-%d")
    )

    if (length(idx) == 0) {
      rv$results <- rbind(rv$results, new_row)
    } else {
      rv$results[idx[[1]], ] <- new_row
    }
    rv$results <- normalize_results(rv$results)

    if (set_selected) {
      rv$selected <- list(fluc = f$fluc, mu = f$mu, sigma = f$sigma, symbol = sym)
    }

    list(ok = TRUE, symbol = sym)
  }

  observeEvent(input$go, {
    result <- refresh_ticker(input$symbol)
    if (!result$ok) {
      showNotification(result$message, type = "error")
      return()
    }

    write.csv(rv$results, file_path, row.names = FALSE)
    idx <- which(toupper(trimws(rv$results$Symbol)) == result$symbol)
    session$sendCustomMessage(
      "updateRiskTableRow",
      list(symbol = result$symbol, rowData = table_row_data(rv$results, idx[[1]]))
    )
  })

  observeEvent(input$scan_all, {
    rv$results <- normalize_results(rv$results)
    symbols <- unique(toupper(trimws(rv$results$Symbol)))
    symbols <- symbols[!is.na(symbols) & symbols != ""]

    if (length(symbols) == 0) {
      showNotification("There are no tickers in the table to scan.", type = "warning")
      return()
    }

    refreshed <- character()
    failed <- character()
    withProgress(message = "Scanning tickers", value = 0, {
      for (i in seq_along(symbols)) {
        incProgress(1 / length(symbols), detail = symbols[[i]])
        result <- refresh_ticker(symbols[[i]], set_selected = FALSE)
        if (result$ok) {
          refreshed <- c(refreshed, result$symbol)
        } else {
          failed <- c(failed, symbols[[i]])
        }
      }
    })

    if (length(refreshed) > 0) {
      write.csv(rv$results, file_path, row.names = FALSE)
      for (sym in refreshed) {
        idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
        session$sendCustomMessage(
          "updateRiskTableRow",
          list(symbol = sym, rowData = table_row_data(rv$results, idx[[1]]))
        )
      }
    }

    message <- paste("Updated", length(refreshed), "of", length(symbols), "tickers.")
    if (length(failed) > 0) {
      message <- paste(message, "Skipped:", paste(failed, collapse = ", "))
    }
    showNotification(message, type = if (length(failed) > 0) "warning" else "message")
  })
  
  # -------------------------
  # Table
  # -------------------------
  
  output$table <- renderDT({
    datatable(
      isolate(rv$results),
      selection = "single",
      editable = list(
        target = "cell",
        disable = list(columns = c(0:9))
      ),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        scrollX = TRUE,
        stateSave = TRUE,
        autoWidth = FALSE,
        rowCallback = JS(
          "function(row, data) {
            var refpRaw = data[4];
            var curpRaw = data[5];
            var refp = Number(refpRaw);
            var curp = Number(curpRaw);
            var hasRefp = refpRaw !== null && refpRaw !== '' && refpRaw !== 'NA' && isFinite(refp);
            var hasCurp = curpRaw !== null && curpRaw !== '' && curpRaw !== 'NA' && isFinite(curp);
            $(row).removeClass('refp-above-curp refp-below-curp');
            if (!hasRefp || !hasCurp || refp === curp) return;
            $(row).addClass(refp > curp ? 'refp-above-curp' : 'refp-below-curp');
          }"
        ),
        columnDefs = list(
          list(
            targets = 0,
            render = JS(
              "function(data, type, row, meta) {
                if (type !== 'display') return data;
                var current = String(data || 'zNA');
                var opts = ['Tier1', 'Tier2','Tier3', 'zNA'];
                var html = '<select class=\"bank-select\">';
                for (var i = 0; i < opts.length; i++) {
                  var selected = opts[i] === current ? ' selected' : '';
                  html += '<option value=\"' + opts[i] + '\"' + selected + '>' + opts[i] + '</option>';
                }
                html += '</select>';
                return html;
              }"
            )
          ),
          list(
            targets = 2,
            width = "56px",
            render = JS(
              "function(data, type, row, meta) {
                if (type !== 'display') return data;
                var value = Number(data);
                if (!isFinite(value)) value = 0;
                 return '<input class=\"IWgt-input\" type=\"text\" inputmode=\"decimal\" ' +
                         'style=\"width:40px;padding:2px;\" ' +
                         'value=\"' + value + '\">';
              }"
            )
          ),
          list(
            targets = 4,
            width = "70px",
            render = JS(
              "function(data, type, row, meta) {
                if (type !== 'display') return data;
                var value = (data === null || data === '' || data === 'NA') ? NaN : Number(data);
                var display = isFinite(value) ? value.toFixed(1) : '';
                return '<input class=\"refp-input\" type=\"text\" inputmode=\"decimal\" ' +
                       'style=\"width:60px;padding:2px;\" value=\"' + display + '\">';
              }"
            )
          ),
          list(
            targets = c(3, 5, 6, 7, 8),
            render = JS("$.fn.dataTable.render.number(',', '.', 2)")
          )
        )
      )
      
    )
  }, server = FALSE)

  observeEvent(input$table_bank_change, {
    
    e <- input$table_bank_change
    sym <- toupper(trimws(e$symbol))
    bank <- as.character(e$value)

    if (!bank %in% c("Tier1", "Tier2","Tier3", "zNA")) {
      bank <- "zNA"
    }

    rv$results <- normalize_results(rv$results)
    idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
    if (length(idx) == 0) return()

    rv$results$Bank[[idx[[1]]]] <- bank
    rv$results <- normalize_results(rv$results)
    write.csv(rv$results, file_path, row.names = FALSE)
  })

  observeEvent(input$table_iwgt_change, {
    
    e <- input$table_iwgt_change
    sym <- toupper(trimws(e$symbol))
    iwgt <- as.numeric(e$value)

    if (is.na(iwgt)) {
      iwgt <- 0
    }

    rv$results <- normalize_results(rv$results)
    idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
    if (length(idx) == 0) return()

    rv$results$IWgt[[idx[[1]]]] <- iwgt
    rv$results <- normalize_results(rv$results)
    write.csv(rv$results, file_path, row.names = FALSE)
  })

  observeEvent(input$table_refp_change, {
    e <- input$table_refp_change
    sym <- toupper(trimws(e$symbol))
    refp <- suppressWarnings(as.numeric(e$value))

    rv$results <- normalize_results(rv$results)
    idx <- which(toupper(trimws(rv$results$Symbol)) == sym)
    if (length(idx) == 0) return()

    rv$results$RefP[[idx[[1]]]] <- if (is.na(refp)) NA_real_ else round(refp, 1)
    rv$results <- normalize_results(rv$results)
    write.csv(rv$results, file_path, row.names = FALSE)
  })
  
  observeEvent(input$table_rows_selected, {
    
    req(length(input$table_rows_selected) == 1)
    i <- get_table_row(input$table_rows_selected)
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
