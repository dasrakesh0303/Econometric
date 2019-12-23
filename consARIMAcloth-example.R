#
# Consumer spending data - 
# Quarterly data on UK expenditures 1963:1 to 2008:4

library("tseries")
library("urca")
library("forecast")
source(file="intord.R")
library("dynlm")
library("lmtest")

df = read.csv("conspend.csv")

dfts = ts(df, start=c(1963,1), frequency=4)
plot.ts(dfts)
lcloth = log(dfts[,3]) # clothing and footwear exp.
lcloth = window(lcloth,end=c(2005,4)) # removing 3 years (12 obs)
Acf(lcloth)
Pacf(lcloth)

#unit root test
intord(lcloth)
yt=ur.df(lcloth, type="trend",selectlags="BIC")
summary(yt)


# Detrend and deseasonalize

dummies = seasonaldummy(lcloth)
res1 = dynlm(lcloth~trend(lcloth)+dummies+L(lcloth,4)+L(lcloth,8)+L(lcloth,12))
summary(res1)
intord(res1$residuals)
Acf(res1$residuals)
Pacf(res1$residuals)

n=length(lcloth)
tr = (1:n)/4
res1b = dynlm(lcloth~tr+dummies+L(lcloth,4)+L(lcloth,8)+L(lcloth,12))
summary(res1b)



res=auto.arima(lcloth)
res
Acf(lcloth)
Pacf(lcloth) # unit root problem

dlcloth = diff(lcloth) # difference with previous quarter (solve unit root problem)
sddlcloth = diff(dlcloth,4) # difference with previous year (solve unit root problem on seasonal lags)
Acf(dlcloth)
Pacf(dlcloth)


Acf(sddlcloth)
Pacf(sddlcloth)

#estimate ARIMA model

arma1 = arima(lcloth,order=c(1,1,1),seasonal=c(1,1,1))
arma1

arma2 = arima(lcloth,order=c(0,1,0),seasonal=c(1,1,1))
arma2

arma3 = arima(lcloth,order=c(0,1,1),seasonal=c(1,1,1))
arma3

res # auto.arima function - optimal results

Acf(res$residuals)
Pacf(res$residuals)

Acf(arma2$residuals) # still have spikes for MA(1)
Pacf(arma2$residuals) # still have spikes for AR(1)
arma4 = arima(lcloth,order=c(1,1,0),seasonal=c(1,1,1))
arma4
Acf(arma4$residuals)
Pacf(arma4$residuals)

# diagnostics for residuals

tsdiag(arma4)
# BoX-Ljung Q Statistic
b = Box.test(arma4$residuals,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(arma4$residuals,lag = i, type="Ljung-Box")
blt[i]=b$p.value
}
blt

#
# Out-of-sample forecasting h-steps ahead (dynamic)
#

h = 12 #(3 years ahead)
n = length(lcloth)
nh = n + h

#create a trend
trt = (1:nh)/4

arma4 = arima(lcloth,order=c(1,1,0),seasonal=c(1,1,1))
#forecasting
fcast = predict(arma4,n.ahead=12)
#plot
ts.plot(lcloth,fcast$pred,col=1:2)
abline(v=2006)
accuracy(arma4)
summary(res)


##
# compare with auto arima
##
arma5 = arima(lcloth,order=c(0,1,1),seasonal=c(1,1,1))
#forecasting
fcast5 = predict(arma5,n.ahead=12)
#plot
ts.plot(lcloth,fcast5$pred,col=1:2)
abline(v=2006)
accuracy(arma5)



#####################################
#########################################
########################################



#
# Introduce trend and seasonal dummies in forecasting
#

#create a trend
trt = (1:nh)/4
trf = trt[(n+1):nh]

sdum = seasonaldummy(ts(rep(0,nh),start=c(1963,1),frequency=4))
sdumf = sdum[(n+1):nh,]
tr=(1:n)/4
xx=cbind(sdum[1:n,],tr)
#arma5 = arima(lcloth,order=c(1,1,0),seasonal=c(1,1,1),xreg=xx)
#arma5 # remove I(1)

arma6 = arima(lcloth,order=c(1,0,0),seasonal=c(1,0,1),xreg=xx)
arma6

Acf(arma6$residuals) # 
Pacf(arma6$residuals)
xxf = cbind(sdumf,trf)
fcast6 = predict(arma6,n.ahead=12,newxreg=xxf)
ts.plot(lcloth,fcast6$pred,col=1:2)
abline(v=2006)





#create a trend
trt = (1:nh)/4
trf = trt[(n+1):nh]

sdum = seasonaldummy(ts(rep(0,nh),start=c(1963,1),frequency=4))
sdumf = sdum[(n+1):nh,]
tr=(1:n)/4
xx=sdum[1:n,]

arma7 = arima(lcloth,order=c(1,0,0),seasonal=c(1,0,1),xreg=xx)
arma7

xxf = sdumf
fcast7 = predict(arma7,n.ahead=12,newxreg=xxf)
ts.plot(lcloth,fcast7$pred,col=1:2)
abline(v=2006)


arma9 = arima(lcloth,order=c(1,0,0),seasonal=c(1,0,1))
arma9

Acf(arma9$residuals) # 
Pacf(arma9$residuals)




xx=tr


arma8 = arima(lcloth,order=c(1,0,0),seasonal=c(1,0,1),xreg=xx)
arma8

xxf = trf
fcast8 = predict(arma8,n.ahead=12,newxreg=xxf)
ts.plot(lcloth,fcast8$pred,col=1:2)
abline(v=2006)
arma7

