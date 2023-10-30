install.packages('xts')
library(xts)

covid <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/kansas-history.csv')

#plot separated by one day, lag one
plot(covid$positive[1:364]/10^3,covid$positive[2:365]/10^3,
     xlab="Day t",ylab="Day t+1")

#lags up to 120
acf(covid$positive,lag.max = 120,type = "correlation")

#estimating ar(x) x+1 parameters
#moving average, auto regressive with high number of parameters (high order)

#arma - combination of auto regressive and moving average

#partial autocorrelaton function - pacf = remove the intermediate values (i.e. lag between 1 and 10 directly)

#ar(1) typically has a slowly decaying lag (correlation) and a large pacf value (correlation) at lag 1

#ma(1) first autocorrelation significant, pacf decay exponentially
#ma(2) first two autocorrelations significant, pacf decay exponentially

#if neither the acf or the pacf cuts off (both decay out) then the model is an arma model

#acf for ma
#pacf for ar

#-----
library(TSA)
library(tseries)
data('color')
plot(color,ylab='color property',xlab='batch',type='o')
adf.test(color)

## plot ACF
acf(color,lag.max = 15)

#if decreasing slowly (linear of less), non-staionary
#if decreasing quickly = stationary
#in this case, at lag = 2, becomes non-significant, so acf of a stationary process

## plot PACF
pacf(color,lag.max = 15)
#used to determine order of autoregressive model
#in this case, ar(1)

#fit an arima model
fit=arima(color,order =c(1,0,0))
fit

#model diagnostics
plot(rstandard(fit),ylab='Standardized Residuals',type='o')
hist(rstandard(fit))
qqnorm(residuals(fit))
qqline(residuals(fit))
acf(rstandard(fit))


Box.test(residuals(fit),lag=10,type="Ljung",fitdf=1)
#fitdf = number of ar() model
#bigger the p-value, the better
#since the p − value = 0.3973 > .05 the null hypothesis that the model is correct cannot be rejected. 
#So the Box-Ljung test does not suggest any problems with the model

#this model is overfit
fit2=arima(color,order =c(2,0,0))
fit2
#The hypothesis that the additional parameter,φ2, is equal to 0 is not rejected since twice the standard error is larger
#than the parameter estimate (2 ∗ 0.1815 > 0.1005).

#plots look good, but still overfit
plot(rstandard(fit2),ylab='Standardized Residuals',type='o')
hist(rstandard(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2))
acf(rstandard(fit2))

#ar(1), ma(1)
fit3=arima(color,order =c(1,0,1))
fit3
#the hypothesis that the additional parameter equals 0 cannot be rejected (2 ∗ 0.2742 > −0.1467).
#do not need ma(1), too

#predictions
pred = predict(fit,n.ahead=10)
plot(fit,n.ahead=10,type='b',col="red",ylab='color property',xlab='batch')

#short-cut
library(forecast)
library(lmtest)
best_model <- auto.arima(color)
best_model

coeftest(best_model)

checkresiduals(best_model)

#NON STATIONARY
rates = read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/fredgraph.csv')
attach(rates)

plot(MORTGAGE30US,col="red",main="30 Yr Fixed Rate Mortgage Average(monthly: Jan 1990 - Feb 2021)")

#plot differences
plot(diff(MORTGAGE30US),col="blue",main="Difference in 30 Yr Mortgage Rates",type="o")


plot(MORTGAGE30US,col="red",main="30 Yr Fixed Rate Mortgage Average(monthly: Jan 1990 - Feb 2021)")
acf(MORTGAGE30US,lag.max = 36,type = "correlation")
#example of a non-stationary acf plot

pacf(MORTGAGE30US)

#acf plot of a differenced time series
acf(diff(MORTGAGE30US),lag.max = 36,type = "correlation")

acf(diff(MORTGAGE30US),lag.max = 36,type = "correlation")
pacf(diff(MORTGAGE30US))
#ar = 2, ma = 2

rates_best <- auto.arima(rates$MORTGAGE30US)
rates_best

rates_next <- arima(rates$MORTGAGE30US, c(2,1,1))
rates_next



