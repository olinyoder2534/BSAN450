library(stats)
library(forecast)
library(TSA)

# QUESTION 1
sales <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/souvenir.csv')

# Plot the time series
plot.ts(sales)
acf(sales)
pacf(sales)
# There is an obvious seasonal component, an increasing mean and non-constant variance

# Perform a Box-Cox Transformation

salesBoxCox.2 <- BoxCox.ar(as.ts(sales$Sales))
# Ideal lambda
salesBoxCox.2$mle

# Using Yule-Walker method
salesBoxCox <- BoxCox.ar(sales$Sales, method = c("yule-walker"))
# Ideal lambda
salesBoxCox$mle
# Result: use a ^-.2 transformation

# Using OLS method
salesBoxCox.1 <- BoxCox.ar(sales$Sales, method = c("ols"))
# Ideal lambda
salesBoxCox.1$mle
# Result: use a log transformation

# Which transformation you use depends on which method (yule-walker or ols) you choose

# I am going to use a log transformation
plot.ts(log(sales))
acf(log(sales))
# There is still a seasonal component and the mean is increasing, but the non-constant variance is no longer an issue.
# Additionally, the acf does not die out which is another indicator that the series is non-stationary

# Check the first difference 
plot.ts(diff(log(sales$Sales)))
acf(diff(log(sales$Sales)), lag = 40)
pacf(diff(log(sales$Sales)), lag = 40)
# From the acf plot, we can see significant autocorrelations every 12 lags meaning that a 12th difference is necessary

# Check the first & 12th difference
plot.ts(diff(diff(log(sales$Sales)), lag = 12))
acf(diff(diff(log(sales$Sales)), lag = 12), lag = 40)
pacf(diff(diff(log(sales$Sales)), lag = 12), lag = 40)
# The series appears to be approximately stationary now

# What if we just took the 12th difference?
plot.ts(diff(log(sales$Sales), lag = 12))
acf(diff(log(sales$Sales), lag = 12), lag = 40)
pacf(diff(log(sales$Sales), lag = 12), lag = 40)
# The series is approximately stationary

# Fitting a model
# Model 1: Using just the 12th difference
# Based on the significant correlations from the acf and pacf plots, I fit a AR(2) * SAR(1) with a deterministic trend and 12 difference
# Without the SAR(1) part, there would be a signficiant lingering autocorrelation around lag = 12
salesTrend <- seq_along(log(sales$Sales))
salesModel1 = arima(log(sales$Sales), order=c(2,0,0), seasonal = list(order=c(1,1,0),
                                                             period=12), xreg = salesTrend)
salesModel1
coeftest(salesModel1)
checkresiduals(salesModel1)
# There are no major issues with the model

# Model 2: Using a first & 12th difference
# Based on the significant correlations from the acf and pacf plots, I fit a MA(1) * SMA(1) with a first and 12 difference
# However, the acf and pacf plots are a bit ambiguous
salesModel2 <- arima(log(sales$Sales), order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
salesModel2
coeftest(salesModel2)
checkresiduals(salesModel2)
# There are no major issues with the model

# The model using just the 12th difference had a lower sigma^2, aic, and a higher log-likelihood, so it is likely the better model

# Check for outliers
detectIO(salesModel1)
detectAO(salesModel1)

# There is a an additive outlier at time = 15
AO15 =1*(seq(sales$Sales)==15)
salesxreg = data.frame(AO15)

# Estimating the effects of the outlier
salesModel1.ao = arimax(log(sales$Sales),order=c(2,0,0),salesxreg,seasonal=list(order=c(1,1,0),period=12))
salesModel1.ao
coeftest(salesModel1.ao)
# The additive outlier at t = 15 is not significant. Additionally, sigma^2 is now lower. 
checkresiduals(salesModel1.ao)
# There is a significant lingering autocorrelation at lag 4. Other than that, there are no issues

# Check for outliers again
detectIO(salesModel1.ao)
detectAO(salesModel1.ao)
# No further outliers detected

# Ultimately, the best model is salesModel1 even though it does not estimate the effects of the outlier.

##########
##########
##########
##########

# METHOD 2 - TAKE A ^-.2 TRANSFORMATION
sales2 <- sales
sales2$Sales <- sales2$Sales^-.2

plot.ts(sales2$Sales)
acf(sales2$Sales)
# There is still a seasonal component and the mean is increasing, but the non-constant variance is no longer an issue.
# Additionally, the acf does not die out which is another indicator that the series is non-stationary

# Check the first difference 
plot.ts(diff(sales2$Sales))
acf(diff(sales2$Sales), lag = 40)
pacf(diff(sales2$Sales), lag = 40)
# From the acf plot, we can see significant autocorrelations every 12 lags meaning that a 12th difference is necessary

# Check the first & 12th difference
plot.ts(diff(diff(sales2$Sales), lag = 12))
acf(diff(diff(sales2$Sales), lag = 12), lag = 40)
pacf(diff(diff(sales2$Sales), lag = 12), lag = 40)
# The series appears to be approximately stationary now

# What if we just took the 12th difference?
plot.ts(diff(sales2$Sales, lag = 12))
acf(diff(sales2$Sales, lag = 12), lag = 40)
pacf(diff(sales2$Sales, lag = 12), lag = 40)
# The series is approximately stationary

# Fitting a model
# Model 1: Using just the 12th difference

# MA(2) * SMA(1)
sales2Trend <- seq_along(sales2$Sales)
sales2Model1 = arima(sales2$Sales, order=c(0,0,2), seasonal = list(order=c(0,1,1),
                                                                      period=12), xreg = sales2Trend)
sales2Model1
coeftest(sales2Model1)
checkresiduals(sales2Model1)
# There are no major issues with the model

# AR(2) * SAR(1)
sales2Model2 = arima(sales2$Sales, order=c(2,0,0), seasonal = list(order=c(1,1,0),
                                                                       period=12), xreg = sales2Trend)
sales2Model2
coeftest(sales2Model2)
checkresiduals(sales2Model2)

# AR(2) * SMA(1)
sales2Model3 = arima(sales2$Sales, order=c(2,0,0), seasonal = list(order=c(0,1,1),
                                                                       period=12), xreg = sales2Trend)
sales2Model3
coeftest(sales2Model3)
checkresiduals(sales2Model3)

# Model 2: Using a first & 12th difference
sales2Model4 <- arima(sales2$Sales, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1), period = 12))
sales2Model4
coeftest(sales2Model4)
checkresiduals(sales2Model4)
# There are no major issues with the model

sales2Model5 <- arima(sales2$Sales, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12))
sales2Model5
coeftest(sales2Model5)
checkresiduals(sales2Model5)
# There are no major issues with the model

detectIO(sales2Model1)
detectAO(sales2Model1)

# The model using just the 12th difference had a lower sigma^2, aic, and a higher log-likelihood, so it is likely the better model

##############
##############
##############
##############
##############
##############

# QUESTION 2
employ <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/emales.CSV')

plot.ts(employ)
acf(employ)
pacf(employ)
# There is a seasonal component and increasing mean

# There is no need to do a transformation since variance is constant
# -----
employBoxCox <- BoxCox.ar(employ$Employed)
employBoxCox$mle

employBoxCox1 <- BoxCox.ar(employ$Employed, method = c('yule-walker'))
employBoxCox1$mle

employBoxCox2 <- BoxCox.ar(employ$Employed, method = c('ols'))
employBoxCox2$mle
#-----

# Check the first difference 
plot.ts(diff(employ$Employed))
acf(diff(employ$Employed), lag = 40)
pacf(diff(employ$Employed), lag = 40)
# From the acf plot, we can see significant autocorrelations every 12 lags meaning that a 12th difference is necessary

# Check the first & 12th difference
plot.ts(diff(diff(employ$Employed),lag=12))
plot.ts(diff(diff(employ$Employed), lag = 12))
acf(diff(diff(employ$Employed),lag=12), lag = 40)
pacf(diff(diff(employ$Employed),lag=12), lag = 40)
# The series appears to be approximately stationary now, maybe an AR(1) * SAR(2) or MA(1) * SMA(1)

# What if we just took the 12th difference?
plot.ts(diff(employ$Employed, lag = 12))
acf(diff(employ$Employed, lag = 12), lag = 40)
pacf(diff(employ$Employed, lag = 12), lag = 40)
# The series is does not appear to be stationary

# Fitting a model
## DO NOT DO THIS
#####
# Model 1: Using just the 12th difference
# Based on the significant correlations from the acf and pacf plots, I fit a AR(2) * SAR(1) with a deterministic trend and 12 difference
employTrend <- seq_along(employ$Employed)
employModel1 = arima(employ$Employed, order=c(2,0,0), seasonal = list(order=c(2,1,0),
                                                                      period=12), xreg = employTrend)
employModel1
coeftest(employModel1)
checkresiduals(employModel1)
# A AR(2) * SAR(2) fit better than a AR(2) * SAR(1)
# There are no major issues with the model

# Less intuitive model, but better comparitive statistics
employModel1.1 = arima(employ$Employed, order=c(1,0,1), seasonal = list(order=c(0,1,1),
                                                                      period=12), xreg = employTrend)
employModel1.1
coeftest(employModel1.1)
checkresiduals(employModel1.1)
# There are no major issues with the model
#####

# DO THIS
# Model 2: Using a first & 12th difference
# AR(1) * SAR(2)
employModel2 <- arima(employ$Employed, order = c(1, 1, 0), seasonal = list(order = c(2, 1, 0), period = 12))
employModel2
coeftest(employModel2)
checkresiduals(employModel2)
# There are no major issues with the model

# MA(1) * SMA(1)
employModel3 <- arima(employ$Employed, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
employModel3
coeftest(employModel3)
checkresiduals(employModel3)
# There are no major issues with the model

#AR(1) * SMA(1)
employModel4 <- arima(employ$Employed, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
employModel4
coeftest(employModel4)
checkresiduals(employModel4)
# There are no major issues with the model

# Ultimately, I decided to go with the AR(2) * SAR(2) model with a deterministic trend

# Check for outliers
detectIO(employModel3)
detectAO(employModel3)

# There may be an additive or innovative outlier at t = 18 & t = 21
# Innovative
employModel3.io = arimax(employ$Employed,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),
                         io = c(18, 21))
employModel3.io
coeftest(employModel3.io)
checkresiduals(employModel3.io)
# No major issues with the model

# Recheck for outliers
detectIO(employModel3.io)
detectAO(employModel3.io)
# No further outliers detected
