#setwd("C:/classes/TimeSeries/Rcodes")
df = read.csv("USCons.csv")
library("quantmod")
library("tseries")
dft <- ts(df[,2:4],frequency=4,start=c(1985,2))
plot(dft)

lcons = dft[,1]
ldisp = dft[,2]
lprice = dft[,3]
source(file="intord.R")
intord(lcons)
intord(ldisp)
intord(lprice)

res=lm(lcons~ldisp+lprice) 
summary(res)
library(lmtest)
bgtest(res, order = 1) # problem of autocorrelation in error terms


# use lag of lcons
library(dynlm)
res2=dynlm(lcons~L(lcons,1)+ldisp+lprice)
summary(res2)

bgtest(res2, order = 1)
bgtest(res2, order = 2)
bgtest(res2, order = 3) # robust for the first 3 lags only
bgtest(res2, order = 4)
bgtest(res2, order = 5)
bgtest(res2, order = 6)


# variables not stationary take the first difference
res3=lm(diff(lcons)~diff(ldisp)+diff(lprice))
summary(res3)

bgtest(res3, order = 1) # bg rejected for all lags - serial correlation removed
bgtest(res3, order = 2)
bgtest(res3, order = 3)
bgtest(res3, order = 4)
bgtest(res3, order = 5)
bgtest(res3, order = 6)

