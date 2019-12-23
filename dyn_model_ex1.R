# Aggregate macro data, monthly 2001:1 
#  to 2012:12 (120 observations)

df = read.csv("aggmacro.csv")
library("quantmod")
library("urca")
library("dynlm")
source(file="intord.R")

dfts = ts(df,frequency = 12,start=c(2001,1))
plot.ts(dfts)

unemp = dfts[,4]# Unemployment
inc = dfts[,3] # Total personal income
cpi = dfts[,1] # CPI index

intord(unemp)
# unemp is I(1)

dfunemp=ur.df(unemp,type="trend",selectlags="BIC")
summary(dfunemp) # Fail to reject unit root

dfunemp2=ur.df(unemp,type="drift",selectlags="BIC")
summary(dfunemp2) # Fail to reject unit root

dunemp = diff(unemp)
dfunemp3=ur.df(dunemp,type="trend",selectlags="BIC")
summary(dfunemp3) # variable is stationary

intord(cpi) # I(1)
dfcpi=ur.df(cpi,type="trend",selectlags="BIC")
summary(dfcpi) #might be trend-stationary
#remove the trend (detrend)
res0=dynlm(cpi~trend(cpi))
summary(res0)
cpidetrend = res0$residuals # detrended cpi
dfcpidetrend=ur.df(cpidetrend,type="trend",selectlags="BIC")
summary(dfcpidetrend) #trend is not significant

dcpi = diff(cpi)

intord(inc) #I(1)
dinc = diff(inc)

# dynamic regression
# start with 2 years of lags
res1=dynlm(dunemp~L(dunemp,1:24)+L(dcpi,0:24)+L(dinc,0:24))
summary(res1)
#nothing is significant beyond lag 12

# start with 1 year of lags
res2=dynlm(dunemp~L(dunemp,1:12)+L(dcpi,0:12)+L(dinc,0:12),start=c(2002,2))
summary(res2) # remove 12 months plus one because of the difference;
AIC(res2)
BIC(res2)
res3=dynlm(dunemp~L(dunemp,1:5)+L(dcpi,0:4)+L(dinc,0:12),start=c(2002,2))
summary(res3)
AIC(res3)
BIC(res3)
#remove CPI not significant
res4=dynlm(dunemp~L(dunemp,1:5)+L(dinc,0:12),start=c(2002,2))
summary(res4)
AIC(res4)
BIC(res4)

# Test if dcpi Granger causes dunemp
# Joint F-test comparing res3 and res4
anova(res3,res4,test="F")
#Fail to reject the null
# H0: dcpi and its past lags = 0
# dcpi does not Granger cause dunemp