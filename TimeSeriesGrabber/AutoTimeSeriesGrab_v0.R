library(quantmod)

# Get data (use a real date range)

getSymbols("GOOGL", from = "2024-01-01")

# Extract closing price (do it once)

price <- Cl(GOOGL)

# Plot closing price

plot(price, main = "GOOGL Closing Price")

# ---- Local peaks ----

prev <- lag(price, 1)
nexts <- lead(price, 1)

peaks <- (price > prev) & (price > nexts)

GOOGL_peaks <- GOOGL[peaks]



# plot peaks
plot(Cl(GOOGL_peaks), main = "GOOGL Peak closing price")


# ---- Local troughs ----
troughs <- (price < prev) & (price < nexts)

GOOGL_troughs <- GOOGL[troughs]
plot(Cl(GOOGL_troughs),
     main = "GOOGL Trough Closing Prices")


#Plot daily return
returns <- dailyReturn(Cl(GOOGL)) #Compute returns
hist(returns) 

hist( dailyReturn(Cl(GOOGL_peaks)) )
hist( dailyReturn(Cl(GOOGL_troughs)) )


#Select codes and use CMD+Enter