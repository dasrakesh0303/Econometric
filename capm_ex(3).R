
# Capital Asset Pricing Model

df = read.csv("CAPM2017.csv")
library("quantmod")

#create time series
dfts = ts(df[,2:9],frequency = 12,start=c(2002,1)) #start Jan 2002
plot.ts(dfts)

#create returns for stock price and market
RPG = Delt(dfts[,8])*100 #eigth column in the dataset dfts
RSANDP = Delt(dfts[,5])*100 #fifth column in the dataset dfts

#Calculate the Excess Return for P&G
RF = dfts[,4] # 3 month US bond
ERPG = RPG - RF

#Calculate the Risk Market Premium
ERSANDP = RSANDP - RF

plot.ts(ERSANDP)
plot.ts(ERPG)

#Estimate CAPM
result = lm(ERPG ~ ERSANDP)
summary(result)

#add 2 factors
SMB = dfts[,2] #Small capitalization minus big
HML = dfts[,3] #High (book to market ratio) minus low

#CAPM with 3 factors
result2 = lm(ERPG ~ ERSANDP + SMB + HML)
summary(result2)
AIC(result)
BIC(result) #lower value - prefered model
AIC(result2)
BIC(result2)

#
# Testing for stationarity
#

source(file="intord.R")

#test for the variable RPG (return PG)
intord(RPG[!is.na(RPG)]) #ignore NA in RPG

#time series is stationary:

# no trend in the initial graph (variable in level)

# SD is not decreasing by more than two 
#for model in difference

#Reject the null hypothesis of unit root:


#ADF t-value lags$adf.stat
#round1 round2
#[1,] -13.72 -10.31

#$critvals
#[1] -2.58 -2.88 -3.47

# At the 1% level the Test Statistic (-13.72) is 
#smaller than -3.47. Variable is statonary


# Testing for PG
PG = df[,9]
intord(PG) # bi


# esimating linear regression
install.packages("dynlm")
library("dynlm")

#change length of 1 variable
res3 = dynlm(ERPG ~ ERSANDP + diff(SMB) + HML)
summary(res3)

res3b = lm(ERPG[-1] ~ ERSANDP[-1] + diff(SMB) + HML[-1])
summary(res3b)
