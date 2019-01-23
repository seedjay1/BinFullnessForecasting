library(ggplot2)
library(dplyr)
library(forecast)

library(dplyr)
library(lubridate)

hbf <- read.csv(file="c:\\HistoricalBinFullnessBRI122.csv")

# make good date col
hbf$TransactionDate <- mdy(hbf$TransactionDate)

# set mpid for modeling
mpid <- 30198

#filter & sort for specific mpid
hbf_fil <- subset(hbf, MachinePlacementId == mpid) #3509 is the canonical example
hbf_fil <- hbf_fil[order(hbf_fil$TransactionDate),]

# extract out the perfect bit for analysis
hbf_perfect <- subset(hbf_fil, select=c("DailyVolume"))


#create time series object


#let's try the simple way - weekly seasonality
#hbf_ts <- ts(hbf_perfect, start=c(2016,1,1), frequency = 365.25)
#fit <- ets(hbf_ts)
#fc <- forecast(fit, h=365)
#plot(fc)


#that's bad - let's try a multi-seasonal model
hbf_ts2 <- msts(hbf_perfect$DailyVolume, seasonal.periods = c(365.25))
fit2 <- tbats(hbf_ts2)
fc2 <- forecast(fit2, h=365)
plot(fc2, sub=paste("mpid ", mpid))

#still bad - on to arima
#hbf_ts <- ts(hbf_perfect, 365.25)
#auto.arima(ts(temps,frequency=365))
#hbf_aa <- auto.arima(ts(hbf_ts, frequency = 365.25))
#fc3 <- forecast(hbf_aa, h=365.25)
#plot(fc3)

#let's try a fourier way
hbf_ts <- ts(hbf_perfect, 365.25)
hbf_ts <- msts(hbf_perfect$DailyVolume,seasonal.periods=c(365.25),start=c(2016,1,1))
fit4 <- Arima(hbf_ts, order=c(10,0,6), xreg=fourier(hbf_ts, K=c(4)))
fc4 <- forecast(fit4, xreg=fourier(hbf_ts, K=c(4), h=365))
plot(fc4, sub=paste("mpid ", mpid))

#let's try an AIC minimization model selection
hbf_ts <- ts(hbf_perfect, freq = 365.25/7, start=c(2016,1,1))
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(hbf_ts, xreg=fourier(hbf_ts, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
fc5 <- forecast(bestfit, xreg=fourier(hbf_ts, K=12))
plot(fc5)








