library(forecast)
library(TSA)

#set.seed(1)
#temps <- 20+10*sin(2*pi*(1:856)/365)+arima.sim(list(0.8),856,sd=2)

#plot(forecast(auto.arima(temps),h=365))
#plot(forecast(auto.arima(ts(temps,frequency=365)),h=365))


p = periodogram(hbf$DailyVolume)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 5)

# display the 2 highest "power" frequencies
top2


# convert frequency to time periods
time = 1/top2$f
time