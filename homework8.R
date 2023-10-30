library(TSA)
library(xts)
library(forecast)

#Question 1
milk <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/evapmilk.csv')

head(milk)

#a
plot(milk$Stock, type = 'o')
acf(milk$Stock)

kpss.test(milk$Stock)
adf.test(milk$Stock)

#b
plot(diff(milk$Stock), type = 'o')
acf(diff(milk$Stock))

#c
plot(diff(milk$Stock, lag = 12), type = 'o')
acf(diff(milk$Stock, lag = 12))
pacf(diff(milk$Stock, lag = 12))


#d
plot(diff(diff(milk$Stock, lag = 12)), type = 'o')
acf(diff(diff(milk$Stock, lag = 12)))
pacf(diff(diff(milk$Stock, lag = 12)))

kpss.test(diff(diff(milk$Stock, lag = 12)))
adf.test(diff(diff(milk$Stock, lag = 12)))


milkModel1 <- auto.arima(milk$Stock, seasonal = TRUE)
milkModel1
checkresiduals(milkModel1)
plot(milkModel1,n.ahead=30,type='b')

milkModelx = arima(milk$Stock,order=c(0,1,1),seasonal = list(order=c(0,1,1),
                                                             period=12))
plot(milkModelx, n.ahead = 32)
checkresiduals(milkModelx)

#e
milkModel2 = arima(milk$Stock,order=c(1,0,0),seasonal = list(order=c(0,1,0),
                                                               period=12))
milkModel2
coeftest(milkModel2)
checkresiduals(milkModel2)
plot(milkModel2, n.ahead = 24)
#x follows pattern


milkModeln2 = arima(milk$Stock,order=c(1,0,0),seasonal = list(order=c(0,1,1),
                                                             period=12))
milkModeln2
checkresiduals(milkModeln2)
plot(milkModeln2, n.ahead = 24)
prediction <- predict(milkModeln2, n.ahead = 24)$pred
class(prediction)
ts.plot(as.ts(milk),prediction, col=c("#050000","#E4000E"), main = "(1−φ1B)(1−B12)Yt  = (1−θ12B12)")

prediction$pred


#f
milkModel3 <- arima(milk$Stock, order = c(0,1,0), seasonal = list(order=c(0,1,1), period = 12))
milkModel3
coeftest(milkModel3)
checkresiduals(milkModel3)
plot(milkModel3, n.ahead = 24)

prediction2 <- predict(milkModel3, n.ahead = 24)$pred
ts.plot(as.ts(milk),prediction2, col=c("#050000","#E4000E"), main = "(1 −B)(1 −B12)Yt  = (1−θ12B12)t")

#x approaches 0



#QUESTION 2
gov <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/GovRec.csv')
head(gov)

#use xreg  = 1:length(num observations)
#a
plot(gov$Receipts, type = 'o')

govModel1 <- auto.arima(log(gov$Receipts))
govModel1

#b
TSA::BoxCox.ar(gov$Receipts, method = "yule-walker")

#c
plot(log(gov$Receipts), type = 'o')
acf(log(gov$Receipts))

#d
plot(diff(log(gov$Receipts)), type = 'o')
acf(diff(log(gov$Receipts)))

#e
plot(diff(log(gov$Receipts), lag = 12), type = 'o')
acf(diff(log(gov$Receipts)), lag = 12)
pacf(diff(log(gov$Receipts)), lag = 12)

#f
plot(diff(diff(log(gov$Receipts), lag = 12)), type = 'o')
acf(diff(diff(log(gov$Receipts), lag = 12)))
pacf(diff(diff(log(gov$Receipts), lag = 12)))
#kpss.test(diff(diff(log(gov$Receipts), lag = 12)))
#adf.test(diff(diff(log(gov$Receipts), lag = 12)))
#ma(1)

#g
#sma(1)
govModel2 = arima(log(gov$Receipts),order=c(1,0,1),seasonal = list(order=c(0,1,0),
                                                             period=12),  xreg = 1:length(gov$Receipts))
coeftest(govModel2)
checkresiduals(govModel2)

govModel2
plot(govModel2, n.ahead = 25)

#h
#sma(1)
govModel3 = arima(log(gov$Receipts),order=c(2,1,0),seasonal = list(order=c(1,1,0),
                                                            period=12))
govModel3
coeftest(govModel3)
checkresiduals(govModel3)
plot(govModel3, n.ahead = 25)





# QUESTION 3

college <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/CSrev.csv')
head(college)

plot.ts(college$Revenues)
plot.ts(college$Revenues)
acf(college$Revenues)
pacf(college$Revenues)

plot(diff(college$Revenues), type = 'o')
acf(diff(college$Revenues))
pacf(diff(college$Revenues))

plot(diff(log(college$Revenues)), type = 'o')
acf(diff(log(college$Revenues)))
pacf(diff(log(college$Revenues)))

plot.ts(diff(diff(college$Revenues), lag = 3))
acf(diff(diff(college$Revenues), lag = 3), lag = 49)

plot.ts(diff(diff(college$Revenues), lag = 12))
acf(diff(diff(college$Revenues), lag = 12), lag = 49)
pacf(diff(diff(college$Revenues), lag = 12), lag = 49)
#ma(1)

collegeModel1 <- auto.arima(log(college$Revenues))
collegeModel1
checkresiduals(collegeModel1)


acf(diff(college$Revenues, lag = 12))


#ma(1) * sma(1)
collegeModel2 <- arima(college$Revenues, order=c(0,1,1),seasonal = list(order=c(0,1,1),
                                                                        period=12))
collegeModel2
coeftest(collegeModel2)
checkresiduals(collegeModel2)
# sig^2 = 2721, p-val = .2533

collegeModelx <- arima(college$Revenues, order=c(0,0,1),seasonal = list(order=c(0,1,1),
                                                                        period=12), xreg = 1:length(college$Revenues))
collegeModelx
coeftest(collegeModelx)
checkresiduals(collegeModelx)


plot(collegeModel2, n.ahead = 50)

college2 <- college
college2 <- as.ts(college2)

collegeModel3 <- auto.arima(college2, seasonal = TRUE, stepwise= F)
collegeModel3
coeftest(collegeModel3)

checkresiduals(collegeModel3)

