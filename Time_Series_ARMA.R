# ARMA on US_GDP Dataset
us_gdp = read.csv(file.choose(),header = T)
View(us_gdp)
us_gdp_ts=ts(us_gdp,start = 1960)

## checking stationarity
library(tseries)
adf.test(us_gdp_ts)
# as p-value > 0.05 so the data is non-stationary

#Hypothesis Testing
## H0 : data series is  not stationary
## H1 : data series is stationary
#as p-value is >0.05 there is not enough evidence to reject H0

acf(us_gdp_ts)
pacf(us_gdp_ts)

## converting into stationary data
us_gdp_diff1=diff(us_gdp_ts)
adf.test(us_gdp_diff1)
# as p-value < 0.05 so the data is stationary

# to find the order
acf(us_gdp_diff1)
pacf(us_gdp_diff1)

# to figure correct no. of difference we use forecast package
library(forecast)
ndiffs(x=us_gdp_ts)

plot(diff(us_gdp_ts,2)) ## the graph is significant because 
#variation of upward and downward are equal so it is almost zero.
# plot(us_gdp_ts) # in this graph data moves only upward

## ARIMA(p,d,q)
## p and q should varies from 0-5 while d varies from 0-2
## least AIC value is better

## trying to fit correct model for dataset

usbest = auto.arima(us_gdp_ts)
usbest
## AICC is the corrected and minimum AICC is the best model

acf(usbest$residuals)
pacf(usbest$residuals)
# as there is no bar exceeding the blue line so there are no significant
# and the value are almost near to zero

# coefficients of ARMA model 
coef(usbest)

predict(usbest,n.ahead = 5,se.fit = TRUE)
theforecast = forecast(object = usbest,h = 5)
plot(theforecast)
