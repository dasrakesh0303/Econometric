

#### set a working directory: Session>Set Working Directory>Choose Directory
# or type
####setwd("C:/...")

# load datafile
df=read.csv("example.csv")


install.packages("quantmod")
install.packages("stats")
library("quantmod")
library("stats")

# define the variables
y = df$Y  # dependent variable
x = df$X  # explanatory variable

# scatter plot
plot(x,y)

# run a simple regression
result = lm(y~x)
summary(result)

#
### calculate the predicted y when x=250
#

yhat1 = 15.11+0.61*250
#more precise prediction
yhat2 = result$coefficients[1]+result$coefficients[2]*250

#
# calculate predicted or fitted values
yhat = result$fitted.values
#calculate the residuals
res = y - yhat
res1 = result$residuals
cbind(res,res1)

#plot the fitted line
plot(x,y)
abline(a=result$coefficients[1],b=result$coefficients[2])

# create a new variable x2
set.seed(1234)
x2=rnorm(20)

# estimate the multiple regression

res2= lm(y~x+x2)
summary(res2)

# Test for multiple regression
#install.packages("car")
library(car)

#Testing for H0: interecpt=0 and x=0.5
linearHypothesis(res2,c("(Intercept)=0","x=0.5"))
# Pvalue is 8.9e-14. Therefore we reject H0.

#Testing for H0: x=0.5 and x1=0.5
linearHypothesis(res2,c("x=0.5","x2=0.5"))
# Pvalue is 0.009. Therefore we reject H0


#Testing for H0: x2=3
linearHypothesis(res2,"x2=3")
# Pvalue is 0.92. Therefore we fail to reject H0

