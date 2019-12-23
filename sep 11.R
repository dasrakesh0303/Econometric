df = read.csv("example.csv")
y= df$Y
x= df$X
plot(x,y)
library("stats")
#run a simple regression
result= lm(y~x)
summary(result)
qt(0.025,df=19)
pt(-2.09,df=19)
#calculate predicted y when x=250
yhat1=15.11+0.61*250
#more precision
yhat2=result$coefficients[1]+result$coefficients[2]*250
#calculate predicted or fitted values
yhat=result$fitted.values
#calculate the residuals
res=y-yhat
res1=result$residuals
cbind(res,res1)
#plot the fitted line
abline(a=result$coefficients[1],b=result$coefficients[2])
