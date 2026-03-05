### Read training data
#! Perhaps you need to set the working directory!?
#setwd("/home/pbac/g/course02417/2025/assignment1")


###############################################################
# Professors code
###############################################################

D <- read.csv("assignments/Assignment1/DST_BIL54.csv")
str(D)

# See the help
?strftime
D$time <- as.POSIXct(paste0(D$time,"-01"), "%Y-%m-%d", tz="UTC")
D$time
class(D$time)

## Year to month for each of them
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
D$total <- as.numeric(D$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]

###############################################################
# 1 Plot Data
###############################################################
x <- Dtrain$year
plot(Dtrain$year, Dtrain$total, 
     type = "l",  # line plot
     xlab = "Year", 
     ylab = "Total vehicles (millions)",
     main = "Registered vehicles in Denmark (Training data)")
points(Dtrain$year, Dtrain$total, pch = 16, cex = 0.5) 

# for later, to compare accuracy of prediction
y <- Dtrain$total

















###############################################################
# 3 OLS
###############################################################
# Design matrix X for linear trend model: y = theta1 + theta2 * x
# Column 1: intercept (ones), Column 2: time variable x


x <- Dtrain$year
y <- as.numeric(Dtrain$total)
cat("Number of training observations:", length(y), "\n")
X <- cbind(1, x)
# OLS estimate: theta_hat = (X'X)^(-1) X'y
thetahat <- solve(t(X) %*% X) %*% (t(X) %*% y)
cat("OLS Parameter Estimates:\n")
cat("theta1 (intercept):", thetahat[1], "\n")
cat("theta2 (slope):    ", thetahat[2], "\n")

###############################################################
# 3.2 Standard errors and plot
###############################################################

# we get y estimate (y hat) and residual (how good the fit is)
yhat_train <- X %*% thetahat
 residuals <- y - yhat_train
n <- length(y)
p <- ncol(X)

#get sigma hat squared to calculate Var(theta_hat) 
# Var(theta_hat) = sigma^2 * (X'X)^(-1)
sigma2_hat <- sum(residuals^2) / (n - p)
sigma_hat <- sqrt(sigma2_hat)
cat("Estimated residual standard deviation (sigma):", sigma_hat, "\n\n")
var_thetahat <- sigma2_hat * solve(t(X) %*% X)
se_theta <- sqrt(diag(var_thetahat)) #take sqrt of diaginal elements for SE

cat("Parameter Estimates with Standard Errors:\n")
cat("theta1 =", round(thetahat[1], 4), " +/- ", round(se_theta[1], 4), "\n")
cat("theta2 =", round(thetahat[2], 4), " +/- ", round(se_theta[2], 4), "\n")


plot(x, y, 
     pch = 16, col = "blue",
     xlab = "Year", 
     ylab = "Total vehicles (millions)",
     main = "Global Linear Trend Model - Training Data")
lines(x, yhat_train, col = "red", lwd = 2)
legend("topleft", 
       legend = c("Observations", "Estimated mean"), 
       col = c("blue", "red"), 
       pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

###############################################################
# 3.3 Forecast for test set (12 months ahead)
###############################################################

x_test <- Dtest$year
y_test <- Dtest$total
X_test <- cbind(1, x_test)
#Actual predictions
yhat_test <- X_test %*% thetahat

#Calculate variance of the prediction error for each test point
XtX_inv <- solve(t(X) %*% X)
pred_var <- sapply(1:nrow(X_test), function(i) {
  x_new <- X_test[i, ]
  sigma2_hat * (1 + t(x_new) %*% XtX_inv %*% x_new)
})
pred_se <- sqrt(pred_var)

# decided on taking alpha=5% for 95% prediction intervals
# not actually sure
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = n - p)
lower <- yhat_test - t_crit * pred_se
upper <- yhat_test + t_crit * pred_se

# to view results
results <- data.frame(
  Month = format(Dtest$time, "%Y-%b"),
  Observed = round(y_test, 4),
  Predicted = round(yhat_test, 4),
  Lower_95 = round(lower, 4),
  Upper_95 = round(upper, 4)
)

head(results, 12)


# Plot forecast with prediction intervals
plot(c(x, x_test), c(y, y_test),
     type = "n",
     xlab = "Year", 
     ylab = "Total vehicles (millions)",
     main = "Forecast with 95% Prediction Intervals")

points(x, y, pch = 16, col = "blue")
lines(x, yhat_train, col = "red", lwd = 2)
points(x_test, y_test, pch = 17, col = "darkgreen", cex = 1.2)
lines(x_test, yhat_test, col = "red", lwd = 2)


lines(x_test, lower, col = "red", lty = 2)
lines(x_test, upper, col = "red", lty = 2)
print(upper)
#polygon(c(x_test, rev(x_test)), c(lower, rev(upper)), col = rgb(1, 0, 0, 0.2), border = NA)

        
legend("topleft", 
       legend = c("Training data", "Test data", "Fitted/Forecast", "95% PI"), 
       col = c("blue", "darkgreen", "red", "red"), 
       pch = c(16, 17, NA, NA), 
       lty = c(NA, NA, 1, 2), lwd = c(NA, NA, 2, 1))





###############################################################
# 3.5 Residual analysis
###############################################################
# OLS, assumptions are same variance 
# and be mutually uncorrelated
# We need to run tests to check whether this is correct

# ============================================================
# VISUAL DIAGNOSTICS
# ============================================================
par(mfrow = c(2, 2))

# 1. Residuals vs time - check for patterns/trends
plot(x, residuals, 
     pch = 16, col = "blue",
     xlab = "Year", ylab = "Residuals",
     main = "Residuals vs Time")
abline(h = 0, col = "red", lwd = 2)
# Look for: Random scatter around 0. Pattern = time-varying mean

# 2. Residuals vs fitted values - check for constant variance
plot(yhat_train, residuals,
     pch = 16, col = "blue",
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lwd = 2)
# Look for: Random scatter. Funnel shape = heteroscedasticity

# 3. Histogram - check for normality
hist(residuals, breaks = 15, col = "lightblue",
     main = "Histogram of Residuals",
     xlab = "Residuals", probability = TRUE)
curve(dnorm(x, mean = 0, sd = sd(residuals)), add = TRUE, col = "red", lwd = 2)

# 4. QQ-plot - check for normality
qqnorm(residuals, pch = 16, col = "blue", main = "Normal Q-Q Plot")
qqline(residuals, col = "red", lwd = 2)
# Look for: Points on the line. Deviations = non-normality

par(mfrow = c(1, 1))

# 5. ACF plot - check for autocorrelation
acf(residuals, main = "ACF of Residuals", lag.max = 20)
# Look for: Bars within blue dashed lines. Significant bars = autocorrelation

# ============================================================
# FORMAL STATISTICAL TESTS
# ============================================================
cat("\n========== FORMAL TESTS FOR OLS ASSUMPTIONS ==========\n\n")

# --- Test 1: Normality - Shapiro-Wilk Test ---
shapiro_test <- shapiro.test(residuals)
cat("1. NORMALITY - Shapiro-Wilk Test:\n")
cat("   H0: Residuals are normally distributed\n")
cat("   W =", round(shapiro_test$statistic, 4), "\n")
cat("   p-value =", round(shapiro_test$p.value, 4), "\n")
if (shapiro_test$p.value > 0.05) {
  cat("   >> PASS: Cannot reject normality (p > 0.05)\n\n")
} else {
  cat("   >> FAIL: Residuals are NOT normal (p < 0.05)\n\n")
}

# --- Test 2: Autocorrelation - Ljung-Box Test ---
# Test at multiple lags
ljung_box_10 <- Box.test(residuals, lag = 10, type = "Ljung-Box")
ljung_box_20 <- Box.test(residuals, lag = 20, type = "Ljung-Box")

cat("2. AUTOCORRELATION - Ljung-Box Test:\n")
cat("   H0: Residuals are uncorrelated (no autocorrelation)\n")
cat("   Lag 10: Q =", round(ljung_box_10$statistic, 4), 
    ", p-value =", round(ljung_box_10$p.value, 4), "\n")
cat("   Lag 20: Q =", round(ljung_box_20$statistic, 4), 
    ", p-value =", round(ljung_box_20$p.value, 4), "\n")
if (ljung_box_10$p.value > 0.05 & ljung_box_20$p.value > 0.05) {
  cat("   >> PASS: Cannot reject independence (p > 0.05)\n\n")
} else {
  cat("   >> FAIL: Residuals ARE autocorrelated (p < 0.05)\n\n")
}

# --- Test 3: Homoscedasticity - Breusch-Pagan Test ---
# Manual implementation (or use lmtest package if available)
# Regress squared residuals on X
residuals_sq <- residuals^2
bp_model <- lm(residuals_sq ~ x)
bp_r2 <- summary(bp_model)$r.squared
bp_stat <- n * bp_r2  # Test statistic ~ chi-squared(1)
bp_pvalue <- 1 - pchisq(bp_stat, df = 1)

cat("3. CONSTANT VARIANCE - Breusch-Pagan Test:\n")
cat("   H0: Variance is constant (homoscedasticity)\n")
cat("   BP =", round(bp_stat, 4), "\n")
cat("   p-value =", round(bp_pvalue, 4), "\n")
if (bp_pvalue > 0.05) {
  cat("   >> PASS: Cannot reject constant variance (p > 0.05)\n\n")
} else {
  cat("   >> FAIL: Variance is NOT constant (p < 0.05)\n\n")
}

cat("=======================================================\n")



###############################################################
# 4 WLS - Weighted Least Squares (Local Trend Model)
###############################################################

# 4.1 Variance-covariance matrix comparison
###############################################################
Describe the variance-covariance matrix (the N ×N matrix Σ (i.e. 72 ×72 matrix, so present
only relevant parts of it)) for the local model and compare it to the variance-covariance matrix
of the corresponding global model

lambda <- 0.9
N <- n  # number of observations (72)
weights <- lambda^((N-1):0)
cat("WLS with forgetting factor lambda =", lambda, "\n\n")
cat("Weights (first 5 - oldest observations):\n")
print(round(weights[1:5], 6))
cat("\nWeights (last 5 - newest observations):\n")
print(round(weights[(N-4):N], 6))

# Weight matrix W (diagonal)
W <- diag(weights)
Sigma_local <- sigma2_hat * solve(W)  # sigma^2 * W^(-1)


# to compare how values evolve over time 
cat("Top-left 5x5 corner of Sigma_local (oldest observations):\n")
print(round(Sigma_local[1:5, 1:5], 6))

cat("\nBottom-right 5x5 corner of Sigma_local (newest observations):\n")
print(round(Sigma_local[(N-4):N, (N-4):N], 6))

###############################################################
# 4.2 Plot lambda-weights vs time
###############################################################
plot(x, weights,
     type = "b", pch = 16, col = "blue",
     xlab = "Year", ylab = "Weight",
     main = expression(paste("WLS Weights vs Time (", lambda, " = 0.9)")))
max_weight_idx <- which.max(weights)
points(x[max_weight_idx], weights[max_weight_idx], 
       pch = 19, col = "red", cex = 2)
abline(h = 1, col = "gray", lty = 2)
abline(h = 0.5, col = "gray", lty = 3)


###############################################################
# 4.3 Sum of lambda-weights
###############################################################

sum_wls_weights <- sum(weights)
sum_ols_weights <- N  
sum_wls_weights

###############################################################
# 4.4 WLS Parameter Estimation (lambda = 0.9)
###############################################################
# WLS estimate: theta_hat = (X'WX)^(-1) X'Wy
# where W = diag(weights)

# Calculate X'WX and X'Wy
XtWX <- t(X) %*% W %*% X
XtWy <- t(X) %*% W %*% y
# WLS estimate
thetahat_wls <- solve(XtWX) %*% XtWy
cat("theta1 (intercept):", thetahat_wls[1], "\n")
cat("theta2 (slope):    ", thetahat_wls[2], "\n")
# OLS estimates
cat("\nComparison with OLS estimates:\n")
cat("            OLS          WLS (lambda=0.9)\n")
cat("theta1: ", round(thetahat[1], 4), "      ", round(thetahat_wls[1], 4), "\n")
cat("theta2: ", round(thetahat[2], 4), "       ", round(thetahat_wls[2], 4), "\n")




plot(x, y, 
     pch = 16, col = "blue",
     xlab = "Year", 
     ylab = "Total vehicles (millions)",
     main = "OLS vs WLS (lambda = 0.9) Fitted Lines")
# OLS 
lines(x, X %*% thetahat, col = "red", lwd = 2)
# WLS 
lines(x, X %*% thetahat_wls, col = "darkgreen", lwd = 2)

legend("topleft", 
       legend = c("Observations", "OLS fit", "WLS fit (lambda=0.9)"), 
       col = c("blue", "red", "darkgreen"), 
       pch = c(16, NA, NA), lty = c(NA, 1, 1), lwd = c(NA, 2, 2))

###############################################################
# 4.5 WLS Forecast for test set (12 months ahead)
###############################################################

yhat_test_wls <- X_test %*% thetahat_wls
# Variance of residuals for WLS: sigma^2_wls = sum(w_i * e_i^2) / (sum(w_i) - p)
residuals_wls <- y - X %*% thetahat_wls
sigma2_hat_wls <- sum(weights * residuals_wls^2) / (sum(weights) - p)
sigma_hat_wls <- sqrt(sigma2_hat_wls)
# Variance-covariance of WLS estimates: Var(theta_wls) = sigma^2 * (X'WX)^(-1)
var_thetahat_wls <- sigma2_hat_wls * solve(XtWX)

# Prediction variance for WLS
# Var(y_new - yhat_new) = sigma^2 * (1 + x_new' (X'WX)^(-1) x_new)
XtWX_inv <- solve(XtWX)

pred_var_wls <- sapply(1:nrow(X_test), function(i) {
  x_new <- X_test[i, ]
  sigma2_hat_wls * (1 + t(x_new) %*% XtWX_inv %*% x_new)
})
pred_se_wls <- sqrt(pred_var_wls)

# Same as above using 5% but needs to be checked
df_wls <- sum(weights) - p
t_crit_wls <- qt(1 - alpha/2, df = df_wls)
lower_wls <- yhat_test_wls - t_crit_wls * pred_se_wls
upper_wls <- yhat_test_wls + t_crit_wls * pred_se_wls

# table to compare resultsa
results_comparison <- data.frame(
  Month = format(Dtest$time, "%Y-%b"),
  Observed = round(y_test, 4),
  OLS_Pred = round(yhat_test, 4),
  WLS_Pred = round(yhat_test_wls, 4),
  WLS_Lower = round(lower_wls, 4),
  WLS_Upper = round(upper_wls, 4)
)

cat("\nForecast Comparison Table (values in millions of vehicles):\n")
print(results_comparison)

# ============================================================
# Training data + OLS and WLS forecasts on test set
# ============================================================

plot(c(x, x_test), c(y, y_test),
     type = "n",
     xlab = "Year", 
     ylab = "Total vehicles (millions)",
     main = "OLS vs WLS Forecasts with 95% Prediction Intervals")
points(x, y, pch = 16, col = "blue")
# OLS 
lines(x, X %*% thetahat, col = "red", lwd = 2)
# WLS 
lines(x, X %*% thetahat_wls, col = "darkgreen", lwd = 2)
# Actual OBservatiosn
points(x_test, y_test, pch = 17, col = "black", cex = 1.3)

# OLS forecast line and prediction intervals
lines(x_test, yhat_test, col = "red", lwd = 2)
#lines(x_test, lower, col = "red", lty = 2)
#lines(x_test, upper, col = "red", lty = 2)
#polygon(c(x_test, rev(x_test)), c(lower, rev(upper)), col = rgb(1, 0, 0, 0.15), border = NA)

# WLS forecast line and prediction intervals
lines(x_test, yhat_test_wls, col = "darkgreen", lwd = 2)
lines(x_test, lower_wls, col = "darkgreen", lty = 2)
lines(x_test, upper_wls, col = "darkgreen", lty = 2)
polygon(c(x_test, rev(x_test)), c(lower_wls, rev(upper_wls)), 
        col = rgb(0, 0.5, 0, 0.15), border = NA)

abline(v = 2024, col = "gray", lty = 3)

legend("topleft", 
       legend = c("Training data", "Test data (actual)", 
                  "OLS forecast", "OLS 95% PI",
                  "WLS forecast", "WLS 95% PI"), 
       col = c("blue", "black", "red", "red", "darkgreen", "darkgreen"), 
       pch = c(16, 17, NA, NA, NA, NA), 
       lty = c(NA, NA, 1, 2, 1, 2), 
       lwd = c(NA, NA, 2, 1, 2, 1))
rmse_ols <- sqrt(mean((y_test - yhat_test)^2))
rmse_wls <- sqrt(mean((y_test - yhat_test_wls)^2))
rmse_ols
rmse_wls





















###############################################################
# 5. Recursive Least Squares 
###############################################################

############## 5.2 question
exists("x")
exists("y")

# Initial data - use small R0 to match section 5.3
R0 <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)
theta0 <- matrix(c(0, 0), nrow = 2)
R_list <- list(R0)
theta_list <- list(theta0)
# first 3 true values needed to calculate the error
y_rls <- y[1:3]
x_rls <- x[1:3]  # Use actual year values, not loop index
for (t in 1:3) {
  x_t <- matrix(c(1, x_rls[t]), nrow = 2)  # Use actual year value!
  R_prev <- R_list[[t]]
  theta_prev <- theta_list[[t]]
  #update steps
  R_t <- R_prev + x_t %*% t(x_t)
  e_t <- y_rls[t] - t(x_t) %*% theta_prev
  theta_t <- theta_prev + solve(R_t) %*% x_t %*% e_t
  
  R_list[[t + 1]] <- R_t
  theta_list[[t + 1]] <- theta_t
  
  cat("--- Iteration t =", t, "(year =", x_rls[t], ") ---\n")
  cat("Prediction error e_t =", as.numeric(e_t), "\n")
  cat("R_t =\n"); print(R_t)
  cat("theta_t =\n"); print(theta_t)
  cat("  theta1 (intercept) =", theta_t[1], "\n")
  cat("  theta2 (slope)     =", theta_t[2], "\n\n")
}



######################## 5.3 question

N <- length(y)
# Testing of different initial values for R matrix, to see if we can get the 
# theta 2 parameter to decrease and get closer to OLS estimate

# Define initial R0 values to test
R0_values <- c(0.1, 0.001, 1e-6)
results_table <- data.frame(
  R0_diag = numeric(length(R0_values)),
  RLS_theta1 = numeric(length(R0_values)),
  RLS_theta2 = numeric(length(R0_values)),
  OLS_theta1 = numeric(length(R0_values)),
  OLS_theta2 = numeric(length(R0_values)),
  Diff_theta1 = numeric(length(R0_values)),
  Diff_theta2 = numeric(length(R0_values))
)

for (i in seq_along(R0_values)) {
  r0_val <- R0_values[i]
  R_t <- matrix(c(r0_val, 0, 0, r0_val), nrow = 2, byrow = TRUE)
  theta_t <- matrix(c(0, 0), nrow = 2)
  
  # RLS loop
  for (t in 1:N) {
    x_t <- matrix(c(1, x[t]), nrow = 2)
    R_t <- R_t + x_t %*% t(x_t)
    e_t <- y[t] - t(x_t) %*% theta_t
    theta_t <- theta_t + solve(R_t) %*% x_t %*% e_t
  }
  results_table$R0_diag[i] <- r0_val
  results_table$RLS_theta1[i] <- theta_t[1]
  results_table$RLS_theta2[i] <- theta_t[2]
  results_table$OLS_theta1[i] <- thetahat[1]
  results_table$OLS_theta2[i] <- thetahat[2]
  results_table$Diff_theta1[i] <- theta_t[1] - thetahat[1]
  results_table$Diff_theta2[i] <- theta_t[2] - thetahat[2]
}

# Print nicely formatted table
cat("\n========== RLS vs OLS Comparison for Different R0 Values ==========\n\n")
cat(sprintf("%-12s | %-14s | %-14s | %-14s | %-14s | %-14s | %-14s\n", 
            "R0 diagonal", "RLS theta1", "OLS theta1", "Diff theta1", "RLS theta2", "OLS theta2", "Diff theta2"))
cat(paste(rep("-", 105), collapse = ""), "\n")
for (i in 1:nrow(results_table)) {
  cat(sprintf("%-12s | %-14.4f | %-14.4f | %-14.2e | %-14.6f | %-14.6f | %-14.2e\n",
              format(results_table$R0_diag[i], scientific = TRUE),
              results_table$RLS_theta1[i],
              results_table$OLS_theta1[i],
              results_table$Diff_theta1[i],
              results_table$RLS_theta2[i],
              results_table$OLS_theta2[i],
              results_table$Diff_theta2[i]))
}
cat(paste(rep("-", 105), collapse = ""), "\n")
cat("\n")

#test







################### 5.4 question
#now we implement RLS with forgetting
#we need to multiply the previous R matrix with the forgetting factor lambda at each step
# the highest the lambda parameter, the more we trust previous esttimate

#I want to make a matrix of lambda to test different parameters
lambda_values <- c(0.7, 0.99)
theta_RLS_list <- list()  # Store final results for each lambda

results_forgetting <- data.frame(
  Lambda = numeric(length(lambda_values)),
  RLS_theta1 = numeric(length(lambda_values)),
  RLS_theta2 = numeric(length(lambda_values)),
  WLS_theta1 = numeric(length(lambda_values)),
  WLS_theta2 = numeric(length(lambda_values)),
  Diff_theta1 = numeric(length(lambda_values)),
  Diff_theta2 = numeric(length(lambda_values))
)

theta1_history <- matrix(NA, nrow = N, ncol = length(lambda_values))
theta2_history <- matrix(NA, nrow = N, ncol = length(lambda_values))
colnames(theta1_history) <- as.character(lambda_values)
colnames(theta2_history) <- as.character(lambda_values)

for (i in seq_along(lambda_values)) {
  lambda <- lambda_values[i]
  R_t <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  theta_t <- matrix(c(0, 0), nrow = 2)
  for (t in 1:N) {
    x_t <- matrix(c(1, x[t]), nrow = 2)
    R_t <- lambda * R_t + x_t %*% t(x_t)
    e_t <- y[t] - t(x_t) %*% theta_t
    theta_t <- theta_t + solve(R_t) %*% x_t %*% e_t
    theta1_history[t, i] <- theta_t[1]
    theta2_history[t, i] <- theta_t[2]
  }
  
  theta_RLS_list[[as.character(lambda)]] <- theta_t
  
  weights_i <- lambda^((N-1):0)
  W_i <- diag(weights_i)
  thetahat_wls_i <- solve(t(X) %*% W_i %*% X) %*% (t(X) %*% W_i %*% y)
  results_forgetting$Lambda[i] <- lambda
  results_forgetting$RLS_theta1[i] <- theta_t[1]
  results_forgetting$RLS_theta2[i] <- theta_t[2]
  results_forgetting$WLS_theta1[i] <- thetahat_wls_i[1]
  results_forgetting$WLS_theta2[i] <- thetahat_wls_i[2]
  results_forgetting$Diff_theta1[i] <- theta_t[1] - thetahat_wls_i[1]
  results_forgetting$Diff_theta2[i] <- theta_t[2] - thetahat_wls_i[2]
}

# Print nicely formatted table
cat("\n========== RLS with Forgetting Factor vs WLS ==========\n\n")
cat(sprintf("%-8s | %-14s | %-14s | %-14s | %-14s | %-14s | %-14s\n", 
            "Lambda", "RLS theta1", "WLS theta1", "Diff theta1", "RLS theta2", "WLS theta2", "Diff theta2"))
cat(paste(rep("-", 100), collapse = ""), "\n")
for (i in 1:nrow(results_forgetting)) {
  cat(sprintf("%-8.2f | %-14.4f | %-14.4f | %-14.2e | %-14.6f | %-14.6f | %-14.2e\n",
              results_forgetting$Lambda[i],
              results_forgetting$RLS_theta1[i],
              results_forgetting$WLS_theta1[i],
              results_forgetting$Diff_theta1[i],
              results_forgetting$RLS_theta2[i],
              results_forgetting$WLS_theta2[i],
              results_forgetting$Diff_theta2[i]))
}
cat(paste(rep("-", 100), collapse = ""), "\n")
cat(sprintf("%-8s | %-14.4f | %-14s | %-14s | %-14.6f | %-14s | %-14s\n",
            "OLS", thetahat[1], "-", "-", thetahat[2], "-", "-"))
cat("\n")

 

# Plot theta1 over time for both lambda values
par(mfrow = c(2, 1))

plot(x, theta1_history[, 1], type = "l", col = "blue", lwd = 2,
     xlab = "Year", ylab = expression(theta[1]),
     main = expression(paste("RLS Estimate of ", theta[1], " (intercept) over time")),
     ylim = range(theta1_history, na.rm = TRUE))
lines(x, theta1_history[, 2], col = "red", lwd = 2)
abline(h = thetahat[1], col = "black", lty = 2, lwd = 1.5)
legend("bottomright", 
       legend = c(expression(paste(lambda, " = 0.7")), 
                  expression(paste(lambda, " = 0.99")),
                  "OLS estimate"),
       col = c("blue", "red", "black"), 
       lty = c(1, 1, 2), lwd = c(2, 2, 1.5))

# Plot theta2 over time for both lambda values
plot(x, theta2_history[, 1], type = "l", col = "blue", lwd = 2,
     xlab = "Year", ylab = expression(theta[2]),
     main = expression(paste("RLS Estimate of ", theta[2], " (slope) over time")),
     ylim = range(theta2_history, na.rm = TRUE))
lines(x, theta2_history[, 2], col = "red", lwd = 2)
abline(h = thetahat[2], col = "black", lty = 2, lwd = 1.5)
legend("topright", 
       legend = c(expression(paste(lambda, " = 0.7")), 
                  expression(paste(lambda, " = 0.99")),
                  "OLS estimate"),
       col = c("blue", "red", "black"), 
       lty = c(1, 1, 2), lwd = c(2, 2, 1.5))

par(mfrow = c(1, 1))

