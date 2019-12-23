library("dynlm")
library("forecast")
library("tseries")
library("quantmod")
library("stats")
library("urca")
source(file="intord.R")
ffrate<-read.csv("ffrate.csv")
ffratets=ts(ffrate[,1:5],frequency = 12,start = c(1982,11))
plot.ts(ffratets)

#testing stationarity
ffr<-ffratets[,1]
gap<-ffratets[,2]
infl<-ffratets[,3]
inflexp<-ffratets[,4]
spread<-ffratets[,5]

#ffr stationarity
ffrn=ur.df(ffr,type="none",selectlags = "BIC")
summary(ffrn) #unit root
ffrt=ur.df(ffr,type="trend",selectlags = "BIC")
summary(ffrt) #no trend
ffr1n=ur.df(diff(ffr),type="none",selectlags = "BIC")
summary(ffr1n) #no unit root
ffr1t=ur.df(diff(ffr),type="trend",selectlags = "BIC")
summary(ffr1t) #no trend
#infl stationarity
infln=ur.df(infl,type="none",selectlags = "BIC")
summary(infln) #no unit root
inflt=ur.df(infl,type="trend",selectlags = "BIC")
summary(inflt) #no trend

dffr<-diff(ffr)
# seasonality
season1<-seasonaldummy(dffr)
r1<-dynlm(dffr~season1)
summary(r1)
season2<-seasonaldummy(infl)
r2<-dynlm(infl~season2)
summary(r2)
#no seasonality at 5% level
# dynamic regression
# start with 2 years of lags
#can avoid seasonal dummy
res1=dynlm(dffr~L(dffr,1:24)+L(infl,0:24))
summary(res1)
AIC(res1)
BIC(res1)
res2=dynlm(dffr~L(dffr,1:23)+L(infl,0:24),start = c(1984,12))
summary(res2)
AIC(res2)
BIC(res2)
plot.ts(res2$residuals)
library(lmtest)
bgtest(res2,order=1) # significant so reject null of no AR meaning there is Autocorrelation
bgtest(res2,order=2) #same
bgtest(res2,order=3) #same
bgtest(res2,order=4) #same
bgtest(res2,order=5) #same
bgtest(res2,order=6) #same
res2arrem<-dynlm(dffr~L(dffr,1:36)+L(infl,0:24),start = c(1984,12)) #remove ar by increasing lag test
summary(res2arrem)
bgtest(res2arrem,order=1) #good
bgtest(res2arrem,order=2) #good
bgtest(res2arrem,order=3) #good
bgtest(res2arrem,order=4) #good
bgtest(res2arrem,order=5) #good
bgtest(res2arrem,order=6) #good
#granger cause
res3=dynlm(dffr~L(dffr,1:36),start = c(1984,12))
summary(res3)
anova(res2arrem,res3,test="F") #reject meaning infl granger cause dffr

