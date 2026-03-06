########################################
# Exercise 2 – Simulating Seasonal Processes
########################################

set.seed(123)

n <- 500
s <- 12   # seasonal period

plot_all <- function(x, title){
    par(mfrow = c(3,1))
    plot.ts(x, main = title, ylab = "")
    
    acf(x, main = "ACF")
    pacf(x, main = "PACF")
}

########################################
# 2.1  (1,0,0) x (0,0,0)_12
# phi1 = 0.6
########################################

x1 <- arima.sim(n=n,
                model=list(ar=0.6))

plot_all(x1, "2.1 AR(1) phi=0.6")


########################################
# 2.2  (0,0,0) x (1,0,0)_12
# Phi1 = -0.9
########################################

ar_season <- rep(0,12)
ar_season[12] <- -0.9

x2 <- arima.sim(n=n,
                model=list(ar=ar_season))

plot_all(x2, "2.2 Seasonal AR(1) Phi=-0.9 (lag 12)")


########################################
# 2.3  (1,0,0) x (0,0,1)_12
# phi1 = 0.9
# Theta1 = -0.7
########################################

ma_season <- rep(0,12)
ma_season[12] <- -0.7

x3 <- arima.sim(n=n,
                model=list(ar=0.9,
                           ma=ma_season))

plot_all(x3, "2.3 AR(1) + Seasonal MA(1)")


########################################
# 2.4  (1,0,0) x (1,0,0)_12
# phi1 = -0.6
# Phi1 = -0.8
########################################

ar_comb <- rep(0,12)
ar_comb[1] <- -0.6
ar_comb[12] <- -0.8

x4 <- arima.sim(n=n,
                model=list(ar=ar_comb))

plot_all(x4, "2.4 AR(1) + Seasonal AR(1)")


########################################
# 2.5  (0,0,1) x (0,0,1)_12
# theta1 = 0.4
# Theta1 = -0.8
########################################

ma_comb <- rep(0,12)
ma_comb[1] <- 0.4
ma_comb[12] <- -0.8

x5 <- arima.sim(n=n,
                model=list(ma=ma_comb))

plot_all(x5, "2.5 MA(1) + Seasonal MA(1)")


########################################
# 2.6  (0,0,1) x (1,0,0)_12
# theta1 = -0.4
# Phi1 = 0.7
########################################

ar_s <- rep(0,12)
ar_s[12] <- 0.7

ma_ns <- -0.4

x6 <- arima.sim(n=n,
                model=list(ar=ar_s,
                           ma=ma_ns))

plot_all(x6, "2.6 MA(1) + Seasonal AR(1)")
