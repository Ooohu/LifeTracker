library(quantmod)
library(PerformanceAnalytics)
library(zoo)

# -----------------------------
# INPUTS
# -----------------------------
symbols <- c("XOM", "SPY")
lookback_days <- 126
roll <- 20

# -----------------------------
# DATA FETCH + VOL FUNCTION
# -----------------------------
get_smooth_vol <- function(symbol) {
  
  getSymbols(symbol, src = "yahoo",
             from = Sys.Date() - 300,
             auto.assign = FALSE) -> px
  
  close_px <- Cl(px)
  
  ret <- dailyReturn(close_px, type = "log")
  
  vol <- runSD(ret, n = roll) * sqrt(252)
  
  colnames(vol) <- symbol
  return(vol)
}

# -----------------------------
# BUILD VOL SERIES
# -----------------------------
vol_list <- lapply(symbols, get_smooth_vol)

vol_xts <- na.omit(merge(vol_list[[1]], vol_list[[2]]))
vol_xts <- tail(vol_xts, lookback_days)

# -----------------------------
# VOL RATIO
# -----------------------------
vol_ratio <- vol_xts[,1] / vol_xts[,2]
colnames(vol_ratio) <- "Vol_Ratio"

vol_ratio_norm <- (vol_ratio - mean(vol_ratio, na.rm = TRUE)) /
  sd(vol_ratio, na.rm = TRUE)

# -----------------------------
# PLOT SETUP
# -----------------------------
par(mfrow = c(2,1), mar = c(4,4,2,2), xpd = NA)

# =============================
# Panel 1: Volatility
# =============================
yl <- range(vol_xts, na.rm = TRUE)

plot(index(vol_xts),
     coredata(vol_xts[,1]),
     type = "l",
     col = "blue",
     lwd = 2,
     ylim = yl,
     main = paste(symbols[1], "vs", symbols[2], "Smoothed Volatility"),
     xlab = "Time",
     ylab = "Annualized Volatility")

lines(index(vol_xts),
      coredata(vol_xts[,2]),
      col = "red",
      lwd = 2)

legend("topright",
       inset = 0.02,
       legend = paste(symbols, "vol"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2,
       bty = "n",
       cex = 0.9)

# =============================
# Panel 2: Normalized Vol Ratio
# =============================

plot(vol_ratio_norm,
     col = "black",
     lwd = 2,
     main = "Normalized Volatility Ratio (Z-score)",
     ylab = "Z-score",
     xlab = "Time")

abline(h = 0, col = "gray", lty = 2)

par(xpd = NA)

legend("topright",
       inset = c(0.02, 0.02),
       legend = paste(symbols[1], "/", symbols[2]," Vol Ratio (Z-score)"),
       col = "black",
       lty = 1,
       lwd = 2,
       bty = "n",
       bg = NA,
       box.col = NA,
       cex = 0.9)

par(xpd = FALSE)