# Create a working directory
# Select Session>Set Working Directory>Choose Directory

# type Ctrl + Enter to run one set of lines at a time

setwd("C:/classes/EtricsFinance/Rcodes")

#load datafile
df=read.csv("USGDP.csv")

#relabel the variable
GDPpc = df$GDPpc

#install a new toolbox
#install.packages("quantmod")
library("quantmod")

#Create new variable using time series
  GDPpcts=ts(GDPpc,frequency = 4,start=c(1947,1)) 
#frequency = 4 (quarter)
#start date = first quarter 1947

plot.ts(GDPpcts,main="gdp per capita")
