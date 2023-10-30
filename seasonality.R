
co2 <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/co2_global.csv')


attach(co2)
plot(average,col="red",
     main="Globally Averaged Monthly CO2 level from Jan 2010 - Dec 2020",
     type='o',xlab='Months')

plot(diff(co2$average), type = "o")

#1st difference for mean, 12th difference for seasonality
#12 difference = yearly seasonal difference

plot(diff(diff(co2$average, lag = 12)), type = 'o')

acf(diff(diff(co2$average,lag = 12)),main="Plot of the 1st and 12th difference")
#ma lag 1 and also spike a lag 12, sma(1), periods = 12

pacf(diff(diff(co2$average,lag = 12)))
#hard to tell, maybe 

#create model
#ma(1)*sma(1)
fit = arima(co2$average, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
fit

checkresiduals(fit)

#predictions 

pred = predict(fit,n.ahead=24)
plot(fit,n.ahead=10,type='b',col="red",ylab='Average CO2',xlab='months')


#summary
# ar(1) = acf decays exponentially; PACF cuts off after lag 1
# ar(2) = acf decays exponentially; PACF cuts off after lag 2
# ma(1) = acf cuts off after lag 1; PACF decays exponentially
# ma(1) = acf cuts off after lag 1; PACF decays exponentially
# arma(1,1) = acf decays exponentially; PACF decays exponentially


# start with ma *sma or ar*sar before doing cross models




