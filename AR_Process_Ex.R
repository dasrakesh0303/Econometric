# ARMA Processes

#
# AR(1) Process
#

set.seed(123)
n=1000
sig2 = 0.5
phi=0.9
e= sqrt(sig2)*rnorm(n) # error terms
y=rep(0,n)
y[1]=e[1]# first observation
for (t in 2:n){
  y[t]=1+phi*y[t-1]+e[t]
}

source(file="intord.R")
intord(y)

acf(y)
pacf(y)

p=1
d=0  
q=0
arima1 = arima(y,order=c(p,d,q))
arima1

#Coefficients:
#  ar1  intercept
#0.3635     1.6819
#s.e.  0.0296     0.0349

# the value 1.6818 is E(y) = mu/(1-phi)=1/(0.6)=1.67



# Unit root Process

set.seed(123)
n=10000
sig2 = 0.5
phi=1
e= sqrt(sig2)*rnorm(n) # error terms
y=rep(0,n)
y[1]=e[1]# first observation
for (t in 2:n){
  y[t]=1+phi*y[t-1]+e[t]
}

source(file="intord.R")
intord(y)

acf(y)
pacf(y)

p=1
d=1  
q=0
arima2 = arima(y,order=c(p,d,q))
arima2

arima3 = arima(diff(y),order=c(1,0,0))
arima3

arima4 = arima(y,order=c(1,1,0),xreg=1:length(y))
arima4
