library(quantmod)
library(dplyr)
library(fitdistrplus)

# Want to get:
## [x] Clean price without daily noise
## [x] Daily fluctuation at a upward trend
## [x] Big drop before the bounce

#######################  Define some useful functions:
find_peaks <- function(data) { #Remove continous drops or rises
  price <- Cl(data)
  prev  <- lag(price, 1)
  nexts <- lead(price, 1)
  peaks <- (price > prev) & (price > nexts)
  peaks[is.na(peaks)] <- FALSE
  peaks_data <- data[peaks]

  return(peaks_data)
  
}

find_troughs <- function(data){ #Remove continous drops or rises
  
  price <- Cl(data)
  prev  <- lag(price, 1)
  nexts <- lead(price, 1)
  trough <- (price < prev) & (price < nexts)
  trough[is.na(trough)] <- FALSE
  troughs_data <- data[trough]
  
  return(troughs_data)
  
}

biggest_drop <- function(clean_price) {
  
  price <- Cl(clean_price)
  
  # running peak
  run_peak <- cummax(price)
  
  # drawdown series
  dd <- price - run_peak
  
  # worst trough (maximum drop point)
  end_i <- which.min(dd)
  end_date <- index(price)[end_i]
  
  # peak before trough
  start_i <- which.max(price[1:end_i])
  start_date <- index(price)[start_i]
  
  # % drop
  start <- as.numeric(price[start_i])
  end   <- as.numeric(price[end_i])
  BigDrop_pct <- (start - end) / start * 100
  
  # output
  data.frame(
    start_date = start_date,
    end_date   = end_date,
    drop_pct   = BigDrop_pct
  )
}

bull_segment <- function(clean_price) {
  
  price <- Cl(clean_price)
  
  # 1. lowest point
  min_i <- which.min(price)
  min_date <- index(price)[min_i]
  min_val <- as.numeric(price[min_i])
  
  # 2. subset AFTER minimum
  after_min <- price[(min_i + 1):NROW(price)]
  
  # guard: if no data after min
  if (length(after_min) == 0) {
    return(list(error = "No data after minimum"))
  }
  
  # 3. peak after minimum
  peak_i_rel <- which.max(after_min)
  peak_i <- min_i + peak_i_rel
  
  peak_date <- index(price)[peak_i]
  peak_val <- as.numeric(price[peak_i])
  
  # 4. output
  list(
    min_date = min_date,
    min_price = min_val,
    peak_after_min_date = peak_date,
    peak_after_min_price = peak_val
  )
}


get_fluctuations <- function(price, min_date, peak_date) {
  
  # subset segment
  segment <- price[paste(min_date, peak_date, sep = "/")]
  
  # previous day values
  prev <- lag(segment, 1)
  
  # daily return (% change)
  drop_pct <- (as.numeric(segment) - as.numeric(prev)) / as.numeric(prev) * 100
  
  # remove NA
  drop_pct <- drop_pct[!is.na(drop_pct)]
}



####################### SAMPLE RUNS BELOW

# Get data (use a real date range)
getSymbols("GOOGL", from = "2026-03-13")

GOOGL_peaks <- find_peaks(GOOGL)
GOOGL_troughs <- find_troughs(GOOGL)



# Plot closing price, peaks and troughs
plot(Cl(GOOGL), main = "GOOGL Closing Price", type= "b")

clean_price <- rbind(GOOGL_peaks, GOOGL_troughs)
clean_price <- clean_price[order(index(clean_price))]
plot(Cl(clean_price), main = "GOOGL Clean Closing Prices", type= "b")
## [x] Clean price without daily noise


plot(Cl(GOOGL_peaks), main = "GOOGL Peak Closing Prices", type= "b")

plot(Cl(GOOGL_troughs), main = "GOOGL Trough Closing Prices", type= "b")



##[x] find biggest drop
biggest_drop(clean_price)
result$drop_pct


##[x] Identify the fluctuations in the bull segment
result = bull_segment(clean_price) ## Get the bull segment first

fluc_pct = get_fluctuations(Cl(clean_price), result$min_date, result$peak_after_min_date)

hist(as.numeric(fluc_pct),
     main = "Distribution of Fluctuations (Bull Segment)",
     xlab = "Peak-Trough Fluctuation (%)",
     col = "lightblue",
     breaks = 30)

min(fluc_pct)



## Get dropping distributions (full range)
price <- Cl(clean_price)

# peak-trough % change
drop_pct <- diff(price) / lag(price) * 100
drop_pct <- drop_pct[!is.na(drop_pct)]

# layout: 2 plots (price + histogram)
par(mfrow = c(2,1))

# 1. price series
plot(price,
     main = "Clean Price",
     col = "black")

# 2. distribution of drops
hist(drop_pct,
     breaks = 50,
     main = "Daily % Change Distribution",
     xlab = "% Change",
     col = "lightblue")


## Fit Gaussian on drop_pct
fit <- fitdistrplus::fitdist(drop_pct, "norm")

# parameters
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]

hist(drop_pct,
     breaks = 50,
     probability = TRUE,
     main = "Return Distribution with Gaussian Fit",
     xlab = "% Change",
     col = "lightblue")

curve(dnorm(x, mean = mu, sd = sigma),
      add = TRUE,
      col = "red",
      lwd = 2)

#Get the left x-value with given p-value p=0.05, using a quantile function
qnorm(0.05, mean = mu, sd = sigma)



#Plot daily return
hist( dailyReturn(Cl(GOOGL)) ) #Compute returns


#Select codes and use CMD+Enter