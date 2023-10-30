
data(faithful)
attach(faithful)

fit.A= lm(waiting~eruptions)
summary(fit.A)

plot(fit.A)
checkresiduals(fit.A)

plot(acf(fit.A$residuals,plot = F)[1:20])
pacf(fit.A$residuals,lag.max = 20)
#lingering correlations, serial dependence

library(TSA)
#ma(1)
fit.B = arimax(waiting,order = c(0,0,1),xreg = data.frame(eruptions),
               method = "ML")
fit.B
coeftest(fit.B)
checkresiduals(fit.B)
plot(rstandard(fit.B),ylab='Standardized Residuals',type='o')
hist(rstandard(fit.B))

#ar(1)
fit.C = arimax(waiting,order = c(1,0,0),xreg = data.frame(eruptions),
               method = "ML")
fit.C
coeftest(fit.C)
checkresiduals(fit.C)

#1. Model A: Residual standard error = 5.914.
#2. Model B: Residual standard error = √31.32 = 5.5964.
#3. Model C: Residual standard error = √31.99 = 5.656

#KWH
kwh <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/kwhourdata.csv')
attach(kwh)

plot(kwh$Kw.hours, type = 'o')

kpss.test(Kw.hours)
adf.test(Kw.hours)
#dickey-fuller test suggest stationary (?)

BoxCox.ar(y=Kw.hours)

plot(log(Kw.hours),col="red",main="Log(Kw.hours)",type='o',xlab='Time')
acf(log(Kw.hours))

#take difference
plot(diff(log(Kw.hours)),col="red",main="diff(Log(Kw.hours))",type='o',xlab='Time')
acf(diff(log(Kw.hours)))

#seasonal difference
plot(diff(log(Kw.hours),lag = 12),col="red",main="diff(Log(Kw.hours),lag=12)",type='o',xlab='Time')
acf(diff(log(Kw.hours),lag = 12))

#take both first and twelfth difference
plot(diff(diff(log(Kw.hours),lag = 12)),col="red",main="diff(diff(Log(Kw.hours),lag=12))",type='o')

acf(diff(diff(log(Kw.hours),lag = 12)))
pacf(diff(diff(log(Kw.hours),lag = 12)))

# model = MA(1)*SMA(1) 
fit.2 = arima(log(kwh$Kw.hours),order=c(0,1,1),seasonal = list(order=c(0,1,1),
                                                               period=12))
fit.2
coeftest(fit.2)
checkresiduals(fit.2)

#input variables
plot(Heat.Days,col="red",type='o',xlb='Time')
plot(Cool.Days,col="blue",type='o',xlb='Time')

plot(log(kwh$Kw.hours)~Heat.Days,col="red")
plot(log(kwh$Kw.hours)~Cool.Days,col="blue")

fit.lm = lm(log(Kw.hours)~Heat.Days+Cool.Days,data=kwh)
summary(fit.lm)
plot(fit.lm)

#seasonal with variables
fit.a = arima(log(Kw.hours),order=c(0,1,0),seasonal = list(order=c(0,1,0),
                                                           period = 12),xreg=data.frame(kwh$Heat.Days,kwh$Cool.Days))
fit.a
coeftest(fit.a)
checkresiduals(fit.a)


fit.b = arima(log(Kw.hours),order=c(0,1,2),seasonal = list(order=c(0,1,1),
                                                           period = 12),xreg=data.frame(kwh$Heat.Days,kwh$Cool.Days))
fit.b
coeftest(fit.b)
checkresiduals(fit.b)
#still looks like seasonal ma


#DETERMINISTIC TREND
kwhIn <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/IndexKwHours.csv')
attach(kwhIn)
plot(KWIndex,col="red",
     main="Index of kilowatt hours of electric used each month for 180 months",type='o',xlab='Months')


e = rnorm(n = 120)
t = seq(1:120)
y = 5 + .2*t + e
plot.ts(y)
acf(y)


plot.ts(diff(y))
acf(diff(y))

mod1 <- lm(y ~ t)
new_e <- y - fitted.values(mod1)

#----
#Decompose a series
library(forecast)

#will show trend, seasonal series, and residual decomp (?)
stil = stats::stl(y, s.window = "periodic", t.window = 13)
plot(stl)
#----


#method1
library(astsa)
n_t = sarima.sim(n = 120, ma = -.3, d = 1)

#method2 
mod2 <- Arima(rnorm(n = 120), order = c(0,1,1), fixed = c(theta = -.3))
n_t <- simulate(mod2, nsim = 120)


yt = 5 +.2*t + n_t
plot.ts(yt)
acf(yt)

plot.ts(diff(yt))
acf(diff(yt))
pacf(diff(yt))

# example 2
modx<- Arima(ts(rnorm(n = 120), frequency = 12), order = c(0,1,1), fixed = c(theta = -.8))
n_tx <- simulate(modx, nsim = 120)

ytx = 5 +.2*t + n_tx

plot.ts(ytx)
acf(ytx)
#pacf(ytx)

plot.ts(diff(ytx))
acf(diff(ytx),lag = 36)

mean(diff(ytx))


#OUTLIERS
savings = read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/savings.csv')
attach(savings)
plot.ts(savings)


fit.1= arima(save,order=c(1,0,0))
plot(rstandard(fit.1),type='o')

#additive, one time events
#innovative, something changes which changes time series
detectAO(fit.1)
detectIO(fit.1)

AO32=1*(seq(save)==32)
AO33=1*(seq(save)==33)
xreg=data.frame(AO32,AO33)
fit.ao= arimax(save,order=c(1,0,0),xreg,seasonal=list(order=c(0,0,0),period=NA))
fit.ao
#both ao32 and 33 are significant
plot(rstandard(fit.ao),type='o')
checkresiduals(fit.ao)

#innovation outliers
fit.io=arimax(save,order=c(1,0,0),io=c(33))
fit.io
checkresiduals(fit.io)

#using library(tsoutliers)
autofit1 = forecast::auto.arima(save)
autofit1


outliers = tsoutliers::tso(y = ts(save), types = c("IO", "AO"))
outliers
plot(outliers)

n = length(save)
index = outliers$outliers$ind

ao = outliers("AO", index[1])
io = outliers("IO", index[2])

ao_effect = outliers.effects(ao, n)
io_effect  = outliers.effects(io,n, pars = coefs2poly(autofit1))






