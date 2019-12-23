# ADF Testing procedures

library(tseries)
#install.packages(urca)
library(urca)
source(file="intord.R")

#set seed
set.seed(1234)
T=100
tr=1:T
u = rnorm(T)
y0 = 1 + 0.2*tr + u # y0 is non stationary

library(dynlm)

res=dynlm(y0~trend(y0))
summary(res)
resb=lm(y0~tr)
summary(resb)

intord(y0)

# ADF TEST with no trend and no drift
dfn=ur.df(y0,type="none",selectlags = "BIC")
summary(dfn)


#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#z.lag.1     0.02210    0.00972   2.274   0.0252
#z.diff.lag -0.49410    0.08960  -5.514 2.95e-07

# The estimate of z.lag.1 represents the value for
# psi. It is not negative therefore the process is
# not stationary.


#Value of test-statistic is: 2.2737 

#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau1 -2.6 -1.95 -1.61

#since the test statistic (2.27) is not 
# smaller than -2.6, we fail to reject the null
# hypothesis of unit root. The process is 
# non-stationary.


#ADF with Drift

dfd=ur.df(y0,type="drift",selectlags = "BIC")
summary(dfd)

#Value of test-statistic is: -0.3322 3.8875 
#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau2 -3.51 -2.89 -2.58
#phi1  6.70  4.71  3.86

# Since the test stat for tau2 (-.3322) is greater 
# than -3.51 then we fail to reject the unit root.
# The process is non stationary
# The test stat for phi1 (3.88) is greater than 3.86
# we only reject the null at the 10% level. The 
# drift and/or the parameter psi are not equal to 0.
# Since the first test fail to reject psi = 0. 
# Therefore we expect the process to have a drift.

# ADF test with drift and trend
dft=ur.df(y0,type="trend",selectlags = "BIC")
summary(dft)
