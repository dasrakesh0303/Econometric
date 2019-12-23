# AR-MAprocess.R

# generate AR(1), AR(2), MA(1) and MA(2) processes
# plot SACF and SPACF for each process

# see ar1sim.R for AR(1) process

# clear the workspace
rm(list = ls())


# AR(2) process

phi1 <- 0.4
phi2 <- 0.3

sig2 <- 0.5
set.seed(128)
n <- 1000
e <- sqrt(sig2)*rnorm(n)
y <- rep(0,n)

y[1] <- e[1]
y[2] <- phi1*y[1] + e[2]
for (t in 3:n) {
  y[t] <- phi1*y[t-1] + phi2*y[t-2] + e[t]
}


par(mfrow=c(2,2))
plot(y[200:300],type='l',col=4,lwd=2)
plot(e[200:300],type='l',col=1,lwd=2)
acf(y,lwd=3,col=3)
pacf(y,lwd=3,col=3)

Acf(y)
Pacf(y)




############# combined AR + MA = ARMA processes next

# ARMA(1,1) process
# y = phi*y(t-1) + e(t) + theta*e(t-1) 

theta <- 0.6
phi <- -0.3
y <- rep(0,n)

y[1] <- e[1]
for (t in 2:n) {
  y[t] <- phi*y[t-1] + e[t] + theta*e[t-1]
}


par(mfrow=c(2,2))
plot(y[200:300],type='l',col=4,lwd=2)
plot(e[200:300],type='l',col=1,lwd=2)
acf(y,lwd=3,col=3)
pacf(y,lwd=3,col=3)

res= auto.arima(y)
res


p <- 5
d <- 0
q <- 2

arma <- arima(y, order = c(p,d,q))
arma
res <- arma$residuals
source(file="intord.R")
intord(res)

p <- 1
d <- 0
q <- 2

arma <- arima(y, order = c(p,d,q))
arma
res <- arma$residuals
source(file="intord.R")
intord(res)


# AR(1) proces

phi1 <- 0.4


sig2 <- 0.5
set.seed(128)
n <- 1000
e <- sqrt(sig2)*rnorm(n)
y <- rep(0,n)

y[1] <- e[1]
for (t in 3:n) {
  y[t] <- 2+phi1*y[t-1]  + e[t]
}

arima = arima(y,order = c(1,0,0)) #intercept is 0.4/(1-phi)
arima

# unit root process

phi1 <- 1


sig2 <- 0.5
set.seed(128)
n <- 1000
e <- sqrt(sig2)*rnorm(n)
y <- rep(0,n)

y[1] <- e[1]
for (t in 3:n) {
  y[t] <- 1+phi1*y[t-1]  + e[t]
}


res= auto.arima(y)
res

arima = arima(y,order = c(2,1,0)) #
arima

arima2 = arima(diff(y),order = c(2,0,0)) #
arima2

arima3 = arima(y,order = c(2,1,0),xreg=1:length(y)) #
arima3




# unit root process

phi1 <- .95


sig2 <- 0.5
set.seed(128)
n <- 1000
e <- sqrt(sig2)*rnorm(n)
y <- rep(0,n)

y[1] <- e[1]
for (t in 3:n) {
  y[t] <- 1+phi1*y[t-1]  + e[t]
}


res= auto.arima(y)
res

arima = arima(y,order = c(2,0,1)) #
arima

arima2 = arima(y,order = c(1,0,1)) #
arima2

arima3 = arima(y,order = c(1,0,0)) #
arima3




