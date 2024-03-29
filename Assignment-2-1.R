library(quantmod)
library(stats)
aptts<-ts(APT[,2:9],frequency = 12,start=c(1986,3))
xomts<-ts(XOM[,2:7],frequency = 12,start=c(1986,3))
plot.ts(aptts)
plot.ts(xomts)
inflation<-Delt(aptts[,3])*100
cpi<-aptts[,3]
Stockpricexom<-xomts[,5]
Stockpricexom
industrialprod<-aptts[,4]
ustb<-aptts[,5]
moneysupply<-aptts[,6]
concredit<-aptts[,7]
spread<-aptts[,8]
sandp<-aptts[,2]
stockregression<-dynlm(Stockpricexom~inflation+industrialprod+ustb+moneysupply+concredit+spread)
summary(stockregression)
plot(Stockpricexom)
stockregression1<-dynlm(Stockpricexom~industrialprod+ustb+moneysupply+concredit+spread)
stockregression2<-dynlm(Stockpricexom~ustb+moneysupply+concredit+spread)
stockregression3<-dynlm(Stockpricexom~industrialprod+moneysupply+concredit+spread)
stockregression4<-dynlm(Stockpricexom~industrialprod+ustb+concredit+spread)
stockregression5<-dynlm(Stockpricexom~industrialprod+ustb+moneysupply+spread)
stockregression6<-dynlm(Stockpricexom~industrialprod+ustb+moneysupply+concredit)
AIC(stockregression)
BIC(stockregression)
AIC(stockregression1)
BIC(stockregression1)
AIC(stockregression2)
BIC(stockregression2)
AIC(stockregression3)
BIC(stockregression3)
AIC(stockregression4)
BIC(stockregression4)
AIC(stockregression5)
BIC(stockregression5)
AIC(stockregression6)
BIC(stockregression6)
source(file="intord.R")
intord(Stockpricexom[!is.na(Stockpricexom)])
intord(inflation[!is.na(inflation)])
intord(industrialprod[!is.na(industrialprod)])
intord(ustb[!is.na(ustb)])
intord(sandp[!is.na(sandp)])
intord(spread[!is.na(spread)])
intord(concredit[!is.na(concredit)])
intord(moneysupply[!is.na(moneysupply)])
Stockpricexom1<-Delt(xomts[,5])*100
excessreturn<-Stockpricexom1-ustb
riskpremium<-(Delt(sandp)*100)-ustb
plot.ts(excessreturn)
plot.ts(riskpremium)
intord(excessreturn[!is.na(excessreturn)])
intord(riskpremium[!is.na(riskpremium)])
returnregression<-dynlm(excessreturn~inflation+spread+diff(riskpremium)+diff(moneysupply)+diff(concredit)+diff(industrialprod)+diff(ustb)+diff(sandp))
summary(returnregression)
#best model
returnregression1<-dynlm(excessreturn~diff(moneysupply)+diff(sandp))
summary(returnregression1)
AIC(returnregression)
BIC(returnregression)
AIC(returnregression1)
BIC(returnregression1)
ansk<-returnregression$coefficients[1]+returnregression$coefficients[4]*0.5
