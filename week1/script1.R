# ----------------------------------------------------------------
# Install packages used
# ----------------------------------------------------------------
# Install packages used
install.packages("mvtnorm")
install.packages("rgl")
#install.packages("httpgd") commented out because it is not on CRAN, so won't work for my version of R, only needed for plots so its chill

# Load the packages
library(mvtnorm)
library(rgl)
# Maybe you like this for plots using VSCode: Open the link printed in a webbrowser or in VSCode plot viewer
#library(httpgd)
#hgd() 
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# The bivariate normal distribution density function
mu <- c(1,0)
Sigma <- matrix(c(1  , 0.7,
                  0.7, 1   ), nrow=2, byrow=TRUE)
# Boundaries for plot
xlim <- c(-5,5)
ylim <- c(-5,5)
#
# Check if Sigma is positive semi definite
eigenvalues <- eigen(Sigma)$values
# and symmetric
if(any(eigenvalues < 0) | any(Sigma != t(Sigma))){
  stop("Sigma is either not positive semi-definite or symmetric")
}
#
# For keeping results
nx <- 1001
x <- seq(xlim[1], xlim[2], length=nx)
y <- seq(ylim[1], ylim[2],length=nx)
xy <- expand.grid(x,y)
# The density
f <- dmvnorm(xy, mu, Sigma)
# The density as a heat map
image(x, y, matrix(f,nrow=nx), xlab="x", ylab="y", main="Joint density f(x,y)")

# As a 3d plot
open3d() ## Note: Do not use rgl.open()
## 'jet.colors' is "as in Matlab", alternatives see ?rainbow
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## Use 100 different colors
colors <- jet.colors(100)
## Set the colors for z values
color <- colors[(f-min(f))/max(f)*99+1]
## Make a surface with jet colors
surface3d(x, y, f, color=color, alpha=1)
# Make a grid on the surface
#surface3d(x, y, f, front="lines", back="lines")
# or make a single colored and transparent surface
#surface3d(x, y, f, color="blue", alpha=0.5)
# Set to get the a nice view
aspect3d(c(1,1,1))
axes3d()
title3d(xlab="x",ylab="y",zlab="f(x,y)")


## ## Make a movie
## ## Set the starting viewpoint
## fov=60
## view3d(0,30,fov)
## M <- par3d("userMatrix")
## M <- rotate3d(M, -pi/2, 1, 0, 0)
## view3d(userMatrix=M,fov=fov)
## ## Run a loop rotating the plot
## start <- proc.time()[3]
## while ((i <- 36*(proc.time()[3]-start)) < 360) {
##   ## Rotate around the z-axis
##   M <- rotate3d(M, pi/200, 0, 0, 1)
##   view3d(userMatrix=M,fov=fov)
## }
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Simulate values
# Set the values again
mu <- c(1,0)
Sigma <- matrix(c(1  , 0.0,
                  0.0,  1  ), nrow=2)
# Boundaries for plot
xlim <- c(-5,5)
ylim <- c(-5,5)
# Simulation from the bi-variate normal
n <- 1000
X <- rmvnorm(n, mu, Sigma)
colnames(X) <- c("x1","x2")

# The empirical covariance
cov(X)
# is an estimate of Sigma
Sigma

# Correlation
cov2cor(cov(X))
cov2cor(Sigma)

# Plot
plot(X[ ,1], X[ ,2], xlim=xlim, ylim=ylim)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Any linear transformation is also a normal distribution
# Derive the second order moment, and then verify by simulation
n <- 1000
A <- matrix(c(2, 30,
              4, 2), nrow=2, byrow=TRUE)
b <- c(1,1)

# Simulate X values
X <- rmvnorm(n, mu, Sigma)

# Transform the simulated values
# Note first
class(X[1, ]) # a "numeric" is a vector and it is treated as a standing vector for matrix operators, e.g. %*%
# Transformed it laying down
t(X[1, ])

# Transform one a single set of values (x1,x2)
A %*% X[1, ] + b
# All of them (we have to a bit of transposition)
X2 <- t(A %*% t(X) + b)
plot(X[ ,1], X[ ,2])

# The mean and covariance of the transformed random vector
mu2 <- ??
Sigma2 <- ??
mu2
Sigma2

# Generate new sample with the tranformed mean and covariance
X2new <- rmvnorm(n, mu2, Sigma2)

# Plot them both
plot(X2[ ,1], X2[ ,2])
points(X2new[ ,1], X2new[ ,2], col="red")
# The means
apply(X2,2,mean)
apply(X2new,2,mean)
# The covariances
cov(X2)
cov(X2new)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Make a linear model: Y = theta_1 * x_1 + theta_2 * x_2 + eps
# where eps = N(0,\sigma^2) and i.i.d.

# As matrices: Y = X theta + eps

# Calculate the conditional distribution and verify by simulation
# Use these values
theta = c(2,3)
sigma <- 2
# Simulated output values
y = X %*% theta + rnorm(n, 0, sigma)

# Derive the mean and variance of Y
# First the mean
??
# Then the variance, it's two not independent variables
??

# Verify the values match approximately with simulation
mean(y)
var(y)
# ------------------------------------------------------#install.packages("rgl")
#install.packages("httpgd")

# Load the packages
library(mvtnorm)
library(rgl)
# Maybe you like this for plots using VSCode: Open the link printed in a webbrowser or in VSCode plot viewer
# library(httpgd)
#hgd() 
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# The bivariate normal distribution density function
mu <- c(1,0)
Sigma <- matrix(c(1  , 0.7,
                  0.7, 1   ), nrow=2, byrow=TRUE)
# Boundaries for plot
xlim <- c(-5,5)
ylim <- c(-5,5)
#
# Check if Sigma is positive semi definite
eigenvalues <- eigen(Sigma)$values
# and symmetric
if(any(eigenvalues < 0) | any(Sigma != t(Sigma))){
  stop("Sigma is either not positive semi-definite or symmetric")
}
#
# For keeping results
nx <- 1001
x <- seq(xlim[1], xlim[2], length=nx)
y <- seq(ylim[1], ylim[2],length=nx)
xy <- expand.grid(x,y)
# The density
f <- dmvnorm(xy, mu, Sigma)
# The density as a heat map
image(x, y, matrix(f,nrow=nx), xlab="x", ylab="y", main="Joint density f(x,y)")

# As a 3d plot
open3d() ## Note: Do not use rgl.open()
## 'jet.colors' is "as in Matlab", alternatives see ?rainbow
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## Use 100 different colors
colors <- jet.colors(100)
## Set the colors for z values
color <- colors[(f-min(f))/max(f)*99+1]
## Make a surface with jet colors
surface3d(x, y, f, color=color, alpha=1)
# Make a grid on the surface
#surface3d(x, y, f, front="lines", back="lines")
# or make a single colored and transparent surface
#surface3d(x, y, f, color="blue", alpha=0.5)
# Set to get the a nice view
aspect3d(c(1,1,1))
axes3d()
title3d(xlab="x",ylab="y",zlab="f(x,y)")


## ## Make a movie
## ## Set the starting viewpoint
## fov=60
## view3d(0,30,fov)
## M <- par3d("userMatrix")
## M <- rotate3d(M, -pi/2, 1, 0, 0)
## view3d(userMatrix=M,fov=fov)
## ## Run a loop rotating the plot
## start <- proc.time()[3]
## while ((i <- 36*(proc.time()[3]-start)) < 360) {
##   ## Rotate around the z-axis
##   M <- rotate3d(M, pi/200, 0, 0, 1)
##   view3d(userMatrix=M,fov=fov)
## }
# ----------------------------------------------------------------






# ----------------------------------------------------------------
# Simulate values
# Set the values again
mu <- c(1,0)
Sigma <- matrix(c(1  , 0.0,
                  0.0,  1  ), nrow=2)
# Boundaries for plot
xlim <- c(-5,5)
ylim <- c(-5,5)
# Simulation from the bi-variate normal
n <- 1000
X <- rmvnorm(n, mu, Sigma)
colnames(X) <- c("x1","x2")

# The empirical covariance
cov(X)
# is an estimate of Sigma
Sigma

# Correlation
cov2cor(cov(X))
cov2cor(Sigma)

# Plot
plot(X[ ,1], X[ ,2], xlim=xlim, ylim=ylim)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Any linear transformation is also a normal distribution
# Derive the second order moment, and then verify by simulation
n <- 1000
A <- matrix(c(2, 30,
              4, 2), nrow=2, byrow=TRUE)
b <- c(1,1)

# Simulate X values
X <- rmvnorm(n, mu, Sigma)

# Transform the simulated values
# Note first
class(X[1, ]) # a "numeric" is a vector and it is treated as a standing vector for matrix operators, e.g. %*%
# Transformed it laying down
t(X[1, ])

# Transform one a single set of values (x1,x2)
A %*% X[1, ] + b
# All of them (we have to a bit of transposition)
X2 <- t(A %*% t(X) + b)
plot(X[ ,1], X[ ,2])

# The mean and covariance of the transformed random vector
mu2 <- A %*% mu + b
Sigma2 <- A %*% Sigma %*% t(A)
mu2
Sigma2

# Generate new sample with the tranformed mean and covariance
X2new <- rmvnorm(n, mu2, Sigma2)

# Plot them both
plot(X2[ ,1], X2[ ,2])
points(X2new[ ,1], X2new[ ,2], col="red")
# The means
apply(X2,2,mean)
apply(X2new,2,mean)
# The covariances
cov(X2)
cov(X2new)
# ----------------------------------------------------------------


# ----------------------------------------------------------------
# Make a linear model: Y = theta_1 * x_1 + theta_2 * x_2 + eps
# where eps = N(0,\sigma^2) and i.i.d.

# As matrices: Y = X theta + eps

# Calculate the conditional distribution and verify by simulation
# Use these values
theta = c(2,3)
sigma <- 2
# Simulated output values
y = X %*% theta + rnorm(n, 0, sigma)

# Derive the mean and variance of Y
# First the mean
??
# Then the variance, it's two not independent variables
??

# Verify the values match approximately with simulation
mean(y)
var(y)
# ------------------------------------------------------