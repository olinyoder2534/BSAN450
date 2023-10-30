library(TSA)
library(forecast)
library(lmtest)
library(tseries)

#Question 1
con <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/ChemicalConcentration.csv')

#a
head(con)
plot(con$Con, type = 'o')
kpss.test(con$Con)
adf.test(con$Con)

#b
par(mfrow = c(1,2))
acf(con$Con)
pacf(con$Con)

#c
model1 <- auto.arima(con$Con)
model1
coeftest(model1)
checkresiduals(model1)

model2 <- arima(con$Con, order = c(2,0,0))
model2
coeftest(model2)
checkresiduals(model2)


#----
a <- base::diff(con)
acf(a$Con)
plot(diff(con$Con), type = 'o')

acf(diff(con$Con))
pacf(diff(con$Con))

mod <- arima(con$Con, order = c(6,1,0))
coeftest(mod)
checkresiduals(mod)
#----



#d
model3 <- arima(con$Con, order = c(2,0,1))
model3
coeftest(model3)
checkresiduals(model3)


model4 <- arima(con$Con, order = c(3,0,0))
model4
coeftest(model4)
checkresiduals(model4)

#e
pred = predict(model2,n.ahead=10)
pred
plot(model2,n.ahead=10,type='b',col="red",ylab='color property',xlab='batch')

#Question 2
time <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/time.csv')
head(time)

#a
plot(time$Time, type = 'o')
kpss.test(time$Time)
adf.test(time$Time)

acf(time$Time)
pacf(time$Time)

timeModel1 <- auto.arima(time$Time)
timeModel1
coeftest(timeModel1)
checkresiduals(timeModel1)

timeModel2 <- arima(time$Time, order = c(3,0,0))
timeModel2
coeftest(timeModel2)
checkresiduals(timeModel2)

timeModel3 <- arima(time$Time, order = c(1,0,1))
timeModel3
coeftest(timeModel3)
checkresiduals(timeModel3)
qqnorm(residuals(timeModel3))
qqline(residuals(timeModel3))

Box.test(residuals(timeModel3),lag=10,type="Ljung",fitdf=2)

timeModel4 <- arima(time$Time, order = c(2,0,0))
timeModel4
coeftest(timeModel4)
checkresiduals(timeModel4)
