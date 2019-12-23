##
## Monthly Housing Data stating Jan 1960
##

df=read.csv("housingstarts.csv")
library("dynlm")
library("forecast")
library("tseries")
library("quantmod")
library("stats")

source(file="intord.R")
hst = ts(df,freq=12,start=c(1960,1))
intord(hst)
# hst is stationary

#regression with seasonal dummies
sea = seasonaldummy(hst)
r1 = dynlm(hst~sea)
summary(r1)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   92.158      3.993  23.082  < 2e-16 ***
#  seaJan        -5.864      5.618  -1.044 0.296954    
#seaFeb        -2.777      5.618  -0.494 0.621296    
#seaMar        30.958      5.618   5.511 5.40e-08 ***
#  seaApr        49.917      5.618   8.886  < 2e-16 ***
#  seaMay        55.368      5.618   9.856  < 2e-16 ***
#  seaJun        53.644      5.618   9.549  < 2e-16 ***
#  seaJul        46.723      5.618   8.317 6.55e-16 ***
#  seaAug        46.287      5.618   8.240 1.17e-15 ***
#  seaSep        38.395      5.618   6.835 2.10e-11 ***
#  seaOct        41.862      5.618   7.452 3.40e-13 ***
#  seaNov        19.707      5.618   3.508 0.000487 ***
#  ---
#
# December effect= 92.158 (benchmark - dummy removed - read the intercept)
# January is not significant
# February is not significant
# March effect = 92.158+31=123

# deseasonalize
intord(r1$residuals)

# analyze seasonal lags
r2 = dynlm(hst~L(hst,1:24)+sea)
summary(r2)

r3 = dynlm(hst~L(hst,1:16)+sea)
summary(r3)

r4 = dynlm(hst~L(hst,1:12)+sea,start=c(1961,1)) # set starting date worth of 1 year of lags
summary(r4)
AIC(r4)
BIC(r4)

r5 = dynlm(hst~L(hst,1:10)+sea,start=c(1961,1)) # set starting date worth of 1 year of lags
summary(r5)
AIC(r5)
BIC(r5)

# test for serial autocorrelation
# Breush Godfrey Test

library(lmtest)
bgtest(r5,order=1) # test with one lag
bgtest(r5,order=2)
bgtest(r5,order=3)
bgtest(r5,order=4)
bgtest(r5,order=5)
bgtest(r5,order=6)


r6 = dynlm(hst~L(hst,1:2)+sea,start=c(1961,1)) # set starting date worth of 1 year of lags
summary(r6)
AIC(r6)
BIC(r6)

bgtest(r6,order=1) # test with one lag
bgtest(r6,order=2)
bgtest(r6,order=3)
bgtest(r6,order=4)
bgtest(r6,order=5)
bgtest(r6,order=6)
