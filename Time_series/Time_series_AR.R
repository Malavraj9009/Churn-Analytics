## AR(n) model
## L is lag operator

## working on exchange rate
library(tseries)

qer=read.csv(file.choose(),header = T)

# converting into TS

qer_ts = ts(qer,start = 1991,frequency = 4)
summary(qer_ts)

adf.test(qer_ts)

## H0 : data series is  not stationary
## H1 : data series is stationary

#as p-value is >0.05 there is not enough evidence to reject H0

d1.qer_ts = diff(qer_ts)
adf.test(d1.qer_ts)

## here is series is stationary as H0 is rejected

## fitting AR(n) model , where n is the order

qer.ar = ar(qer_ts) ## it is for non-stationary
qer.ar$order
qer.ar$x.mean
qer.ar$ar

qer.ar.stat = ar(d1.qer_ts) ## it is stationary
qer.ar.stat$order
qer.ar.stat$x.mean
qer.ar.stat$ar

## FOR NON-STATIONARY
## ACF
acf(qer.ar$resid[-1]) # -1 is mandatory as one value is removed
# 1 has above blue line so it is AR(1) model

## PACF
pacf(qer.ar$resid[-1]) # here also AR(1) is more significant

## FOR STATIONARY
## ACF
acf(qer.ar.stat$resid[-1]) # -1 is mandatory as one value is removed


## PACF
pacf(qer.ar.stat$resid[-1])
