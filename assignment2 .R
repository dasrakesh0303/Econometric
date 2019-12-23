gdp=GDPexcel$gdp
gdpch=GDPexcelchange$gdpch
pcec=pcecexcel$pcec
pcecch=pcecexcelchange$pcecch
library("quantmod")
gdpts=ts(gdp,frequency = 3,start=c(1947,1))
pcects=ts(pcec,frequency = 3,start=c(1947,1))
plot.ts(gdpts,main="gdp")
plot.ts(pcects,main="personal consumption")
gdpchts=ts(gdpch,frequency = 3,start=c(1947,4))
pcecchts=ts(pcecch,frequency = 3,start=c(1947,4))
plot.ts(gdpchts,main="gdp % change")
plot.ts(pcecchts,main="personal consumption % change")
plot(gdp,pcec)
library("stats")
linmodel=lm(pcec~gdp)
summary(linmodel)
abline(a=linmodel$coefficients[1],b=linmodel$coefficients[2])
summary(gdpchts)
sd(gdpchts)
summary(pcecchts)
sd(pcecchts)