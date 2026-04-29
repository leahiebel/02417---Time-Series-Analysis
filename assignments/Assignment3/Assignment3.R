## =========================
## Exercise 2.1: Seasonal AR Model Validation for Solar PV Plant Forecasting
## =========================

## =========================
## 1. Read data
## =========================

dat <- read.csv("assignments/Assignment3/assignment3_2026/datasolar.csv")

# Use measured plant generation explicitly when available
if ("power" %in% names(dat)) {
     y <- dat$power
} else {
     num_cols <- sapply(dat, is.numeric)
     y <- dat[[which(num_cols)[1]]]
}

# Monthly time series
y_ts <- ts(y, frequency = 12)

# Plot original series
plot(y_ts, main = "Monthly solar PV generation", ylab = "MWh", xlab = "Time")

## =========================
## 2. Define model quantities
## =========================

phi1  <- -0.38
Phi1  <- -0.94
mu    <- 5.72
sigma_eps <- 0.22

# Transform data
x <- log(y) - mu
n <- length(x)

## =========================
## 3. Compute one-step-ahead residuals manually
## Model:
## X_t + phi1 X_{t-1} + Phi1 X_{t-12} + phi1*Phi1 X_{t-13} = eps_t
## =========================

res <- rep(NA, n)

for (t in 14:n) {
  res[t] <- x[t] + phi1 * x[t - 1] + Phi1 * x[t - 12] + phi1 * Phi1 * x[t - 13]
}

# Keep only available residuals
res_valid <- na.omit(res)

# Standardized residuals
z <- res_valid / sigma_eps

## =========================
## 4. One-step-ahead fitted values on X-scale
## Xhat_{t|t-1} = -phi1*X_{t-1} - Phi1*X_{t-12} - phi1*Phi1*X_{t-13}
## =========================

xhat <- rep(NA, n)

for (t in 14:n) {
  xhat[t] <- -phi1 * x[t - 1] - Phi1 * x[t - 12] - phi1 * Phi1 * x[t - 13]
}

# Optional: fitted values back on original scale
yhat <- exp(xhat + mu)
## =========================
## 5. Basic numerical checks
## =========================

cat("Number of usable residuals:", length(res_valid), "\n")
cat("Residual mean:", mean(res_valid), "\n")
cat("Residual variance:", var(res_valid), "\n")
cat("Theoretical variance sigma_eps^2:", sigma_eps^2, "\n")

## =========================
## 6. Diagnostic plots
## =========================

par(mfrow = c(2, 2))

plot(res_valid, type = "l",
     main = "Residuals",
     ylab = expression(hat(epsilon)[t]),
     xlab = "t")
abline(h = 0, lty = 2)

acf(res_valid, main = "ACF of residuals")

hist(res_valid, breaks = 10, main = "Histogram of residuals",
     xlab = expression(hat(epsilon)[t]))

qqnorm(res_valid, main = "Normal Q-Q plot")
qqline(res_valid)

par(mfrow = c(1, 1))

## =========================
## 7. Formal tests
## =========================

# Ljung-Box test for no autocorrelation
cat("\n=== LJUNG-BOX TESTS ===\n")
cat("Lag 12:\n")
lb_12 <- Box.test(res_valid, lag = 12, type = "Ljung-Box")
print(lb_12)
lag2 <- min(24, length(res_valid) - 1)
cat("Lag", lag2, ":\n")
lb_lag2 <- Box.test(res_valid, lag = lag2, type = "Ljung-Box")
print(lb_lag2)

# Check for dependence in squared residuals (variance clustering)
cat("\n=== HETEROSKEDASTICITY CHECK ===\n")
cat("ACF of squared residuals:\n")
acf(res_valid^2, main = "ACF of squared residuals")
cat("Ljung-Box test on squared residuals (lag 12):\n")
lb_sq_12 <- Box.test(res_valid^2, lag = 12, type = "Ljung-Box")
print(lb_sq_12)

# Normality test
cat("\n=== NORMALITY TEST ===\n")
shapiro_res <- shapiro.test(res_valid)
print(shapiro_res)

## =========================
## 7b. Consolidated residual diagnostics summary
## =========================

alpha <- 0.05

interpret_p <- function(p_value, null_text) {
     if (is.na(p_value)) {
          return("Not available")
     }
     if (p_value >= alpha) {
          return(paste0("Pass (do not reject H0: ", null_text, ")"))
     }
     paste0("Fail (reject H0: ", null_text, ")")
}

cat("\n=== RESIDUAL TEST SUMMARY (ALL IN ONE PLACE) ===\n")
cat(sprintf("Ljung-Box residuals (lag 12): p = %.4f -> %s\n",
                              lb_12$p.value,
                              interpret_p(lb_12$p.value, "no autocorrelation")))
cat(sprintf("Ljung-Box residuals (lag %d): p = %.4f -> %s\n",
                              lag2,
                              lb_lag2$p.value,
                              interpret_p(lb_lag2$p.value, "no autocorrelation")))
cat(sprintf("Ljung-Box squared residuals (lag 12): p = %.4f -> %s\n",
                              lb_sq_12$p.value,
                              interpret_p(lb_sq_12$p.value, "no ARCH / no variance clustering")))
cat(sprintf("Shapiro-Wilk normality: p = %.4f -> %s\n",
                              shapiro_res$p.value,
                              interpret_p(shapiro_res$p.value, "normal residuals")))

overall_pass <- (lb_12$p.value >= alpha) &&
     (lb_lag2$p.value >= alpha) &&
     (lb_sq_12$p.value >= alpha) &&
     (shapiro_res$p.value >= alpha)

cat(sprintf("Overall residual validation at alpha = %.2f: %s\n",
                              alpha,
                              ifelse(overall_pass,
                                              "Reasonable (assumptions broadly satisfied)",
                                              "Not fully satisfactory (at least one assumption fails)")))


## =========================
## 8. Plot observed vs fitted on original scale
## =========================

plot(y_ts, type = "l", lwd = 2,
     main = "Observed vs one-step-ahead fitted values",
     ylab = "MWh", xlab = "Time")
lines(ts(yhat, frequency = 12), lty = 2, lwd = 2)
legend("topleft", legend = c("Observed", "Fitted"),
       lty = c(1, 2), lwd = 2, bty = "n")




## =========================
## 9. Forecast Y_{t+k|t} for t = 36, k = 1,...,12
## =========================

t0 <- 36
h <- 12

if (n < t0) {
     stop("Not enough observations: need at least 36 values in the series.")
}

# Recursive forecasts on X-scale with E[epsilon_{t+k}|info_t] = 0
x_ext <- rep(NA_real_, t0 + h)
x_ext[1:t0] <- x[1:t0]

for (tt in (t0 + 1):(t0 + h)) {
     x_ext[tt] <- -phi1 * x_ext[tt - 1] - Phi1 * x_ext[tt - 12] - phi1 * Phi1 * x_ext[tt - 13]
}

x_forecast <- x_ext[(t0 + 1):(t0 + h)]
y_forecast <- exp(x_forecast + mu)

# 95% prediction intervals using AR(1) part only:
# (1 + phi1*B) X_t = epsilon_t  =>  X_t = a1 * X_{t-1} + epsilon_t, where a1 = -phi1
a1 <- -phi1
sigma2_eps <- sigma_eps^2
k_vec <- 1:h
var_x_k <- sigma2_eps * (1 - a1^(2 * k_vec)) / (1 - a1^2)
se_x_k <- sqrt(var_x_k)
z975 <- qnorm(0.975)

x_lower <- x_forecast - z975 * se_x_k
x_upper <- x_forecast + z975 * se_x_k

# Transform interval bounds back to power scale
y_lower <- exp(x_lower + mu)
y_upper <- exp(x_upper + mu)

# Build forecast table
if (all(c("year", "month") %in% names(dat))) {
     base_date <- as.Date(sprintf("%04d-%02d-01", dat$year[t0], dat$month[t0]))
     future_dates <- seq(base_date, by = "month", length.out = h + 1)[-1]
     forecast_table <- data.frame(
          k = 1:h,
          year = as.integer(format(future_dates, "%Y")),
          month = as.integer(format(future_dates, "%m")),
          Yhat = as.numeric(y_forecast),
          Lower95 = as.numeric(y_lower),
          Upper95 = as.numeric(y_upper)
     )
} else {
     forecast_table <- data.frame(
          k = 1:h,
          t_index = (t0 + 1):(t0 + h),
          Yhat = as.numeric(y_forecast),
          Lower95 = as.numeric(y_lower),
          Upper95 = as.numeric(y_upper)
     )
}

cat("\n=== 12-MONTH FORECAST FROM t = 36 (POWER SCALE) ===\n")
print(forecast_table, row.names = FALSE)

forecast_csv <- "assignments/Assignment3/forecast_t36_k1_12.csv"
write.csv(forecast_table, forecast_csv, row.names = FALSE)
cat("Forecast table saved to:", forecast_csv, "\n")

# Plot observed series up to t=36 and extend with forecasts for t=37,...,48
y_obs_t0 <- y[1:t0]
y_plot <- ts(c(y_obs_t0, rep(NA_real_, h)), frequency = 12)
y_fore_ts <- ts(c(rep(NA_real_, t0), y_forecast), frequency = 12)
y_lower_ts <- ts(c(rep(NA_real_, t0), y_lower), frequency = 12)
y_upper_ts <- ts(c(rep(NA_real_, t0), y_upper), frequency = 12)

forecast_plot_file <- "assignments/Assignment3/forecast_t36_k1_12.png"
png(filename = forecast_plot_file, width = 1200, height = 700)
plot(y_plot, type = "l", lwd = 2,
           main = "Observed series extended with 12-month forecast (from t = 36)",
           ylab = "MWh", xlab = "Time")
lines(y_fore_ts, col = "red", lty = 2, lwd = 2)
lines(y_lower_ts, col = "blue", lty = 3, lwd = 2)
lines(y_upper_ts, col = "blue", lty = 3, lwd = 2)
legend("topleft", legend = c("Observed (t<=36)", "Forecast (t=37..48)", "95% PI (AR(1) only)"),
                col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 2, bty = "n")
dev.off()

cat("Forecast plot saved to:", forecast_plot_file, "\n")


