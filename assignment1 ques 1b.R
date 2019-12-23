#PredictedVol
Vol1=result$coefficients[1]+result$coefficients[2]*(S.P+0.01)
#increase in predicted vol
cbind(Vol,Vol1)
difference=Vol1-Vol
