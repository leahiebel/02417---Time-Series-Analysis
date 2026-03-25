set.seed(123)

# 2.1 ARIMA(1,0,0) x (0,0,0)_12 model with phi_1 = 0.6

n <- 200  
phi1 <- 0.6

model_2_1 <- arima.sim(
  model = list(ar = phi1),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_1, type = "l", col = "blue",
     main = expression(paste("2.1 ARIMA(1,0,0) with ", phi[1], " = 0.6")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_1, lag.max = 40,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_1, lag.max = 40,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))



# 2.2 ARIMA(0,0,0) x (1,0,0)_12 model with Phi_1 = -0.9
Phi1_2_2 <- -0.9
ar_coef_2_2 <- c(rep(0, 11), Phi1_2_2)

model_2_2 <- arima.sim(
  model = list(ar = ar_coef_2_2),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_2, type = "l", col = "blue",
     main = expression(paste("2.2 ARIMA(0,0,0) x (1,0,0)"[12], " with ", Phi[1], " = -0.9")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_2, lag.max = 48,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_2, lag.max = 48,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))


# 2.3 ARIMA(1,0,0) x (0,0,1)_12 model with phi_1 = 0.9 and Theta_1 = -0.7

phi1_2_3 <- 0.9
Theta1_2_3 <- -0.7

# AR at lag 1, MA at lag 12
ma_coef_2_3 <- c(rep(0, 11), Theta1_2_3)

model_2_3 <- arima.sim(
  model = list(ar = phi1_2_3, ma = ma_coef_2_3),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_3, type = "l", col = "blue",
     main = expression(paste("2.3 ARIMA(1,0,0) x (0,0,1)"[12], " with ", phi[1], " = 0.9, ", Theta[1], " = -0.7")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_3, lag.max = 48,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_3, lag.max = 48,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))


# 2.4 ARIMA(1,0,0) x (1,0,0)_12 model with phi_1 = -0.6 and Phi_1 = -0.8

phi1_2_4 <- -0.6
Phi1_2_4 <- -0.8

# AR coefficients at lags 1, 12, and 13 (interaction term)
ar_coef_2_4 <- c(phi1_2_4, rep(0, 10), Phi1_2_4, -phi1_2_4 * Phi1_2_4)

model_2_4 <- arima.sim(
  model = list(ar = ar_coef_2_4),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_4, type = "l", col = "blue",
     main = expression(paste("2.4 ARIMA(1,0,0) x (1,0,0)"[12], " with ", phi[1], " = -0.6, ", Phi[1], " = -0.8")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_4, lag.max = 48,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_4, lag.max = 48,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))


# 2.5 ARIMA(0,0,1) x (0,0,1)_12 model with theta_1 = 0.4 and Theta_1 = -0.8


theta1_2_5 <- 0.4
Theta1_2_5 <- -0.8

# MA coefficients at lags 1, 12, and 13 (interaction term)
ma_coef_2_5 <- c(theta1_2_5, rep(0, 10), Theta1_2_5, theta1_2_5 * Theta1_2_5)

model_2_5 <- arima.sim(
  model = list(ma = ma_coef_2_5),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_5, type = "l", col = "blue",
     main = expression(paste("2.5 ARIMA(0,0,1) x (0,0,1)"[12], " with ", theta[1], " = 0.4, ", Theta[1], " = -0.8")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_5, lag.max = 48,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_5, lag.max = 48,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))


# 2.6 ARIMA(0,0,1) x (1,0,0)_12 model with theta_1 = -0.4 and Phi_1 = 0.7

theta1_2_6 <- -0.4
Phi1_2_6 <- 0.7

# AR at lag 12, MA at lag 1
ar_coef_2_6 <- c(rep(0, 11), Phi1_2_6)

model_2_6 <- arima.sim(
  model = list(ar = ar_coef_2_6, ma = theta1_2_6),
  n = n
)

par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(model_2_6, type = "l", col = "blue",
     main = expression(paste("2.6 ARIMA(0,0,1) x (1,0,0)"[12], " with ", theta[1], " = -0.4, ", Phi[1], " = 0.7")),
     xlab = "Time", ylab = "Value")
abline(h = 0, lty = 2, col = "gray")

acf(model_2_6, lag.max = 48,
    main = "Autocorrelation Function (ACF)")

pacf(model_2_6, lag.max = 48,
     main = "Partial Autocorrelation Function (PACF)")

par(mfrow = c(1, 1))

