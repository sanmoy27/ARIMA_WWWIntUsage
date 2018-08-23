library(tseries)
library(fpp)
library(devtools)
library(caTools)

View(WWWusage)
plot.ts(WWWusage)

intusage<-WWWusage
##Test for Stationarity
adf.test(intusage)##P-value>0.05, not staionary

##Take Differences=1
intusage1<-diff(intusage, differences=1)
adf.test(intusage1)##P-value>0.05, not staionary

##Take Differences=2
intusage2<-diff(intusage, differences=2)
adf.test(intusage2)##P-value>0.05, significant, staionary


##Determine the order of p
pacf(diff(intusage2))  ##P=3, graph is not sinusoidal

##Determine the order of q
Acf(diff(intusage2)) ##q=0

##Model is (3,2,0)
arima.fit<-Arima(y=intusage, order=c(3,2,0))
arima.fit


intusageforecasts<-forecast(arima.fit, h=8)
plot(intusageforecasts, shaded=TRUE, shadecols="oldstyle")

##check for validation
Acf(intusageforecasts$residuals, lag.max=20)
plot.ts(intusageforecasts$residuals)

Box.test(intusageforecasts$residuals, lag=20, type="Ljung-Box")
##p-value is insignificant, data doesnot exhibit autocorrelation

auto.arima(intusage)
