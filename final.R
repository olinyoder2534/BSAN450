library(forecast)
library(TSA)


#1
sou <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/souvenir.csv')
plot.ts(sou$Sales)
#obvious increasing mean and increasing variance, so we need to take a difference,
#seasonal difference, and transformation

acf(sou$Sales)
acf(sou$Sales, 80)
#ma(4), sma(3)
pacf(sou$Sales)
pacf(sou$Sales, 80)
#ar(0), sar(1)

#get rid of increasing variance
souBox <- BoxCox.ar(y=as.ts(sou$Sales), method = c("yule-walker"))
souBox1 <- BoxCox.ar(sou$Sales, method = "mle")
souBox$ci

sou$Sales <- sou$Sales^-.2
plot.ts(sou$Sales)

acf(sou$Sales)
acf(sou$Sales, 80)
#ma(4), sma(3)
pacf(sou$Sales)
pacf(sou$Sales, 80)
#ar(0), sar(1)

#just 12th difference
plot.ts(diff(sou$Sales, lag = 12))
abline(h = mean(diff(sou$Sales, lag = 12)))
acf(diff(sou$Sales, lag = 12))
acf(diff(sou$Sales, lag = 12), lag = 80)
pacf(diff(sou$Sales, lag = 12), lag = 80)

#look at the first and first and twelfth difference
#mean is around 0, no need to include a trend
plot.ts(diff(diff(sou$Sales, lag = 12)))
#stationary now
kpss.test(diff(sou$Sales))
acf(diff(diff(sou$Sales, lag = 12)), lag = 80)
pacf(diff(diff(sou$Sales, lag = 12)), lag = 80)

#just first difference
plot.ts(diff(sou$Sales))
abline(h = mean(diff(sou$Sales)))
acf(diff(sou$Sales), lag = 80)
pacf(diff(sou$Sales), lag = 80)


x <- diff(diff(sou$Sales, lag = 12))
x1 <- auto.arima(sou$Sales)
checkresiduals(x1)


#deterministic model
trend <- seq_along(sou$Sales)
auto.fit1 <- auto.arima(sou$Sales, d=0, xreg=trend)
auto.fit1
coeftest(auto.fit1)
checkresiduals(auto.fit1)

sou.fit.1 = arima(sou$Sales, order=c(1,0,1), seasonal = list(order=c(0,1,0),
                                                              period=12), xreg = 1:length(sou$Sales))
sou.fit.1
coeftest(sou.fit.1)
checkresiduals(sou.fit.1)

sou.fit.2 = arima(sou$Sales, order=c(0,0,2), seasonal = list(order=c(0,1,1),
                                                             period=12), xreg = trend)
sou.fit.2
coeftest(sou.fit.2)
checkresiduals(sou.fit.2)

detectIO(sou.fit.2)
detectAO(sou.fit.2)

#predictions
plot(sou.fit.2, newxreg=trend)


#best 1st & 12th model
sou2 <- arima(sou$Sales, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 12))
sou2
coeftest(sou2)
checkresiduals(sou2)
#although I initially though an ar(1)-sar(1) model would work best, an ma sma has a lower sigma^2, aic and a much higher log likelihood

#predictions
plot(sou2, n.ahead = 50)

#check for outliers
detectIO(sou2)
detectAO(sou2)

#no outliers

#there is an additive outlier at time 16
AO73=1*(seq(sou$Sales)==16)
xreg=data.frame(AO73)

#estimating the effects of the outlier
fit.ao1= arimax(sou$Sales,order=c(0,1,1),xreg,seasonal=list(order=c(0,1,1),period=12))
fit.ao1
#not significant
coeftest(fit.ao1)
checkresiduals(fit.ao1)
#slightly lower sigma^2 suggesting it is a better model

detectIO(fit.ao1)
detectAO(fit.ao1)
#now, we have an innovative outlier at 74

fit.ao2= arimax(sou$Sales,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12), io = c(74))
fit.ao2

detectIO(fit.ao2)
detectAO(fit.ao2)

#------
sou3 <- arima(sou$Sales, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 12), 
              xreg = 1:length(sou$Sales))
sou3
coeftest(sou3)
checkresiduals(sou3)
#------

#2
males <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/emales.CSV')

head(males)
plot.ts(males)
#obvious seasonality and increasing mean

malesBox <- BoxCox.ar(y=as.ts(males$Employed))
malesBox
#no need for a Box-Cox transformation

acf(males, 100)
pacf(males, 100)
#looks like an ar-sar

#just first
plot.ts(diff(males$Employed))
abline(h = mean(diff(males$Employed)))
acf(diff(males$Employed))
kpss.test(diff(males$Employed))
adf.test(diff(males$Employed))

#just twelfth
plot.ts(diff(males$Employed, lag = 12))
abline(h = mean(diff(males$Employed, lag = 12)))
acf(diff(males$Employed, lag = 12))
pacf(diff(males$Employed, lag = 12), lag = 50)

#first and twelfth
plot.ts(diff(diff(males$Employed, lag = 12)))
#stationary, but there will be an outlier

acf(diff(males$Employed))
pacf(diff(males$Employed))

acf(diff(diff(males$Employed, lag = 12)), 100)
pacf(diff(diff(males$Employed, lag = 12)), 100)

acf(diff(diff(males$Employed, lag = 12)))
pacf(diff(diff(males$Employed, lag = 12)))

fit.1 = arima(males$Employed, order=c(1,0,1), seasonal = list(order=c(0,1,1),
                                                              period=12), xreg = 1:length(males$Employed))
fit.1
coeftest(fit.1)
checkresiduals(fit.1)

detectIO(fit.1)
detectAO(fit.1)

#time 18
male.fit.io2= arimax(males$Employed,order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12), xreg = 1:length(males$Employed), io = c(18))
male.fit.io2
coeftest(male.fit.io2)
checkresiduals(male.fit.io2)

AO18=1*(seq(males$Employed)==18)
male.xreg=data.frame(AO18)

detectIO(male.fit.io2)
detectAO(male.fit.io2)

#estimating the effects of the outlier
male.fit.ao1= arimax(males$Employed,order=c(1,0,1),male.xreg,seasonal=list(order=c(0,1,1),period=12))
male.fit.ao1
coeftest(male.fit.ao1)
checkresiduals(male.fit.ao1)

male2 <- arima(males$Employed, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
male2
coeftest(male2)
checkresiduals(male2)
plot(male2, n.ahead = 50)
#although sar(2) may be significant, it does not improve the aic, log-likelihood, or sigma^2, so I am sticking with sar(1)


male3 <- arima(males$Employed, order = c(1, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12))
male3
coeftest(male3)
checkresiduals(male3)

detectIO(male2)
detectAO(male2)

O18=1*(seq(males$Employed)==18)
O21=1*(seq(males$Employed)==21)
xreg1=data.frame(O18,O21)

fit.1= arimax(males$Employed,order=c(1,1,0),xreg1,seasonal=list(order=c(1,1,0),period=12))
coeftest(fit.1)
fit.1

fit.io=arimax(males$Employed,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12),io=c(18, 21))
fit.io

#the model with innovative outliers seems to fit the best
#fit.io is better


fit.ao2= arimax(sou$Sales,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12), io = c(74))
fit.ao2

#3
