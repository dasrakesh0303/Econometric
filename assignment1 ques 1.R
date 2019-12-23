df=read.csv("hw1.csv")
S.P=df$S.P
Vol=df$Vol
date=df$ï..Date
plot(S.P,Vol)
library(stats)
result= lm(Vol~S.P)
summary(result)
yhat=result$fitted.values
abline(a=result$coefficients[1],b=result$coefficients[2])