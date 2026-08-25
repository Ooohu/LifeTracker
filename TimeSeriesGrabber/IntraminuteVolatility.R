library(shiny)
library(quantmod)
library(xts)

#====================================================
# UI
#====================================================
ui <- fluidPage(
  
  titlePanel("Intraday High-Low Swing Distribution"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "symbol",
        "Ticker",
        value = "SNDK"
      ),
      
      dateInput(
        "from",
        "Start Date",
        value = "2026-06-01"
      ),
      
      selectInput(
        "periodicity",
        "Periodicity",
        choices = c(
          "daily",
          "weekly",
          "monthly",
          "hourly",
          "1minutes",
          "5minutes",
          "15minutes",
          "30minutes",
          "60minutes"
        ),
        selected = "1minutes"
      ),
      
      numericInput(
        "breaks",
        "Histogram bins",
        value = 100,
        min = 10,
        max = 500
      ),
      
      actionButton("go", "Load Data")
      
    ),
    
    mainPanel(
      verbatimTextOutput("status"),
      
      plotOutput("pricePlot", height = "350px"),
      
      hr(),
      
      plotOutput("histPlot", height = "450px")
    )
    
  )
)

#====================================================
# SERVER
#====================================================
server <- function(input, output, session) {
  
  data_reactive <- eventReactive(input$go, {
    
    output$status <- renderText("Downloading data...")
    
    dat <- tryCatch(
      
      getSymbols(
        input$symbol,
        src = "yahoo",
        from = input$from,
        periodicity = input$periodicity,
        auto.assign = FALSE
      ),
      
      error = function(e) e
      
    )
    
    if (inherits(dat, "error")) {
      
      return(list(
        error = TRUE,
        message = dat$message
      ))
      
    }
    
    #--------------------------------------------------
    # Detect whether requested periodicity was honored
    #--------------------------------------------------
    
    idx <- index(dat)
    
    periodicity_used <- "Unknown"
    
    if (length(idx) > 2) {
      
      dt <- median(diff(as.numeric(idx)))
      
      periodicity_used <-
        if (dt < 120) {
          "1-minute"
        } else if (dt < 600) {
          "5-minute"
        } else if (dt < 1800) {
          "15-minute"
        } else if (dt < 3600) {
          "30-minute"
        } else if (dt < 86400) {
          "Hourly"
        } else {
          "Daily or higher"
        }
      
    }
    
    list(
      error = FALSE,
      data = dat,
      periodicity_used = periodicity_used
    )
    
  })
  
  #==================================================
  # Status text
  #==================================================
  output$pricePlot <- renderPlot({
    
    x <- data_reactive()
    
    validate(
      need(!is.null(x), ""),
      need(!x$error, x$message)
    )
    
    data <- x$data
    
    o <- Op(data)
    h <- Hi(data)
    l <- Lo(data)
    c <- Cl(data)
    
    ##=========================================================
    ## Candle Swing
    ##=========================================================
    
    swing <- numeric(NROW(data))
    
    swing[o > c] <- -((h[o > c] / l[o > c]) - 1) * 100
    swing[o < c] <-  ((h[o < c] / l[o < c]) - 1) * 100
    swing[o == c] <- 0
    
    swing <- as.numeric(swing)
    
    ##=========================================================
    ## Detect Market Legs (Simple ZigZag)
    ##=========================================================
    
    threshold <- 0.5        # percent reversal
    
    close <- as.numeric(c)
    high  <- as.numeric(h)
    low   <- as.numeric(l)
    
    pivot <- rep(FALSE, length(close))
    
    direction <- 0
    pivotPrice <- close[1]
    
    extremePrice <- close[1]
    extremeIndex <- 1
    
    for(i in 2:length(close)){
      
      if(direction == 0){
        
        move <- (close[i]-pivotPrice)/pivotPrice*100
        
        if(move >= threshold){
          direction <- 1
          extremePrice <- high[i]
          extremeIndex <- i
        }else if(move <= -threshold){
          direction <- -1
          extremePrice <- low[i]
          extremeIndex <- i
        }
        
        next
      }
      
      ## Up leg
      if(direction == 1){
        
        if(high[i] > extremePrice){
          extremePrice <- high[i]
          extremeIndex <- i
        }
        
        retrace <- (extremePrice-low[i])/extremePrice*100
        
        if(retrace >= threshold){
          
          pivot[extremeIndex] <- TRUE
          
          direction <- -1
          pivotPrice <- extremePrice
          
          extremePrice <- low[i]
          extremeIndex <- i
        }
        
      }else{
        
        ## Down leg
        
        if(low[i] < extremePrice){
          extremePrice <- low[i]
          extremeIndex <- i
        }
        
        retrace <- (high[i]-extremePrice)/extremePrice*100
        
        if(retrace >= threshold){
          
          pivot[extremeIndex] <- TRUE
          
          direction <- 1
          pivotPrice <- extremePrice
          
          extremePrice <- high[i]
          extremeIndex <- i
        }
      }
    }
    
    pivot[extremeIndex] <- TRUE
    
    pivots <- which(pivot)
    
    ##=========================================================
    ## Layout
    ##=========================================================
    
    layout(matrix(1:3,3,1),
           heights=c(1.3,1.1,2.3))
    
    ###########################################################
    ## Panel 1 : Market Legs
    ###########################################################
    
    par(mar=c(0,4,3,2))
    
    plot(index(data),
         close,
         type="n",
         xaxt="n",
         xlab="",
         ylab="Price",
         main="Detected Market Legs")
    
    lines(index(data), close, col="grey80")
    
    if(length(pivots)>=2){
      
      for(i in 2:length(pivots)){
        
        i1 <- pivots[i-1]
        i2 <- pivots[i]
        
        col <- if(close[i2] > close[i1])
          "forestgreen"
        else
          "red"
        
        segments(index(data)[i1],
                 close[i1],
                 index(data)[i2],
                 close[i2],
                 col=col,
                 lwd=3)
        
        
      }
      
      points(index(data)[tail(pivots,1)],
             close[tail(pivots,1)],
             pch=19)
    }

    
    ###########################################################
    ## Panel 2 : Minute Swing
    ###########################################################
    
    par(mar=c(0,4,2,2))
    
    plot(index(data),
         swing,
         type="n",
         xaxt="n",
         xlab="",
         ylab="% Swing",
         main="Minute Swing")
    
    abline(h=0,lty=2)
    
    pos <- swing>=0
    neg <- swing<0
    
    segments(index(data)[pos],0,
             index(data)[pos],swing[pos],
             col="forestgreen",
             lwd=2)
    
    segments(index(data)[neg],0,
             index(data)[neg],swing[neg],
             col="red",
             lwd=2)
    
    ###########################################################
    ## Panel 3 : High / Low
    ###########################################################
    
    par(mar=c(4,4,2,2))
    
    plot(index(data),
         high,
         type="l",
         col="red",
         lwd=2,
         ylim=range(c(high,low)),
         xlab="Time",
         ylab="Price",
         main="High / Low")
    
    lines(index(data),
          low,
          col="blue",
          lwd=2)
    
    legend("topleft",
           c("High","Low"),
           col=c("red","blue"),
           lwd=2,
           bty="n")
    
  })
  
  output$status <- renderText({
    
    x <- data_reactive()
    
    if (is.null(x))
      return("")
    
    if (x$error)
      return(paste("ERROR:", x$message))
    
    requested <- input$periodicity
    detected <- x$periodicity_used
    
    msg <- paste(
      "Rows:", nrow(x$data),
      "\nRequested periodicity:", requested,
      "\nDetected:", detected
    )
    
    if (requested == "1minutes" &&
        detected != "1-minute") {
      
      msg <- paste(
        msg,
        "\n\nWARNING:",
        "Yahoo did not return 1-minute data.",
        "Yahoo usually limits intraday history and may silently return daily data."
      )
      
    }
    
    msg
    
  })
  
  #==================================================
  # Plot
  #==================================================
  
  output$histPlot <- renderPlot({
    
    x <- data_reactive()
    
    validate(
      need(!is.null(x), "")
    )
    
    validate(
      need(!x$error, x$message)
    )
    
    data <- x$data
    
    o <- Op(data)
    h <- Hi(data)
    l <- Lo(data)
    c <- Cl(data)
    
    change <- numeric(NROW(data))
    
    change[o > c] <- -((h[o > c] / l[o > c]) - 1) * 100
    change[o < c] <-  ((h[o < c] / l[o < c]) - 1) * 100
    change[o == c] <- 0
    
    change <- as.numeric(change)
    change <- change[!is.na(change)]
    
    validate(
      need(length(change) > 5,
           "Not enough data.")
    )
    
    p157 <- quantile(change, 0.157)
    p05  <- quantile(change, 0.05)
    p843 <- quantile(change, 0.843)
    
    par(mar=c(5,4,4,1))
    
    hist(
      change,
      breaks=input$breaks,
      main="High-Low Swing Distribution",
      xlab="Swing (%)",
      border="black"
    )
    
    abline(v=0,lty=2)
    abline(v=p157,col="red",lwd=2,lty=2)
    abline(v=p05,col="blue",lwd=2,lty=2)
    abline(v=p843,col="purple",lwd=2,lty=2)
    
    usr <- par("usr")
    
    text(
      p157,
      usr[4] * 0.90,
      paste0("15.7% = ", round(p157, 3), "%"),
      col = "red",
      pos = 4
    )
    
    text(
      p05,
      usr[4] * 0.80,
      paste0("5% = ", round(p05, 3), "%"),
      col = "blue",
      pos = 4
    )
    
    text(
      p843,
      usr[4] * 0.70,
      paste0("84.3% = ", round(p843, 3), "%"),
      col = "purple",
      pos = 4
    )
    
  })
  
}

#====================================================
# Run App
#====================================================
shinyApp(ui, server)

#shiny::runApp("IntraminuteVolatility.R")