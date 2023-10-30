library(ggplot2)
library(stats)
library(MASS)

# QUESTION 1
iron <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/ironcontent.csv')
head(iron)

#a
plot(x = iron$Chem, y = iron$Magn, data = iron)

ggplot(data = iron, aes(x = Magn, y = Chem))+
  geom_point()+
  ggtitle("Chemical vs Magnetic Iron Results")

ggplot(data = iron, aes(x = Magn, y = Chem))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Chemical vs Magnetic Iron Results")

#b
lm1 <- lm(Chem ~ Magn, iron)
summary(lm1)

#c
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
hist(resid(lm1))
shapiro.test(resid(lm1))

#d
summary(lm1)

#f
confint(lm1,level = 0.95)

#g
new <- data.frame(Magn=c(20))
pred_1 <- predict(lm1, new, interval = "prediction", level=0.95)
pred_1

# QUESTION 2
man <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/ManSalary.csv')
head(man)

#a
ggplot(data = man, aes(x = Experience, y = Salary))+
  geom_point()+
  geom_smooth()+
  ggtitle("Salary vs Experience")

cor(man$Experience, man$Salary)
hist(man$Salary, breaks = 5)

#b
lm2 <- lm(Salary ~ Experience, man)
summary(lm2)

#c
plot(lm2, which = 1)

par(mfrow=c(1,1))
hist(resid(lm2))
shapiro.test(resid(lm2))

#d
#box-cox
bc <- boxcox(man$Salary ~ man$Experience)
bc

lambda_bc <- bc$x[which.max(bc$y)]
lambda_bc

#another method
#------
bc1 <- boxcox(lm2)
best_lam <- bc1$x[which.max(bc1$y)]
best_lam

lm2.inverse <- lm((Salary)^best_lam ~ Experience, man)
summary(lm2.inverse)
par(mfrow=c(1,1))
plot(lm2.inverse)
hist(resid(lm2.inverse))
#------

man$ynew <- (man$Salary)^{lambda_bc}

par(mfrow=c(1,1))
plot(log_salary~man$Experience,ylab = 'Salary transformed', main = 'Transformed')
plot(man$Salary ~ man$Experience, main = 'Original')

ggplot(data = man, aes(x = Experience, y = Salary))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Salary vs Experience")

ggplot(data = man, aes(x = Experience, y = ynew))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Salary vs Experience")

#log transformation instead
log_salary <- log(man$Salary, 10)
hist(log_salary, breaks = 5)

lm2_log <- lm(log_salary ~ Experience, man)
summary(lm2_log)

par(mfrow=c(1,1))
plot(lm2_log, which = 1)
hist(resid(lm2_log))
shapiro.test(resid(lm2_log))

#e
lm2_trans <- lm(ynew~Experience, man)
summary(lm2_trans)

par(mfrow=c(2,2))
plot(lm2_trans)

par(mfrow=c(1,1))
hist(resid(lm2_trans))
shapiro.test(resid(lm2_trans))

#predictions
head(man)
newdata<- data.frame(Experience=c(0,1,2,3,9,5,6,8,12))

pred<- predict(lm2, newdata,interval = "prediction",level=0.95)

pred.trans<- predict(lm2_trans ,newdata,interval = "prediction",level=0.95)
head(newdata)

par(mfrow=c(1,1))
plot(newdata$Experience,pred[,1], xlab ="Experience",ylab="Salary",type="l",pch=17,col="blue", xlim = c(0, 15), ylim = c(0,35000))
points(newdata$Experience,pred[,2],type="l",col="red")
points(newdata$Experience,pred[,3],type="l",col="red")

plot(newdata$Experience,{pred.trans[,1]}^{1/lambda_bc},
     xlab ="Experience",ylab="Salary Transformed",type="l",pch=17,col="blue", xlim = c(0, 15), ylim = c(0,35000))
points(newdata$Experience,{pred.trans[,2]}^{1/lambda_bc},
      type="l",col="red")
points(newdata$Experience,{pred.trans[,3]}^{1/lambda_bc},
      type="l",col="red")

# QUESTION 3
breakdown <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/breakdown.csv')
head(breakdown)
dim(breakdown)

#plot
ggplot(data = breakdown, aes(x = Voltage, y = Time))+
  geom_point()+
  geom_smooth()+
  ggtitle("Time vs Voltage")

#linear model
lm3 <- lm(Time ~ Voltage, breakdown)
summary(lm3)

#diagnostics
par(mfrow=c(2,2))
plot(lm3)

par(mfrow=c(1,1))
hist(resid(lm3))
shapiro.test(resid(lm3))

#log transformation
log_time <- log(breakdown$Time, 10)
hist(log_time, breaks = 5)

#shapiro.test(log_time)

ggplot(data = breakdown, aes(x = Voltage, y = log_time))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle("Time vs Voltage")

#model using log transformation
lm3_log <- lm(log_time ~ Voltage, breakdown)
summary(lm3_log)

#diagnostics
par(mfrow=c(2,2))
plot(lm3_log)

par(mfrow=c(1,1))
hist(resid(lm3_log), breaks = 5)
shapiro.test(resid(lm3_log))

cd <- cooks.distance(lm3_log)
cd[1]
cd[3]

#box-cox
bc2 <- boxcox(breakdown$Time ~ breakdown$Voltage)

lambda_bc2 <- bc2$x[which.max(bc2$y)]
lambda_bc2

breakdown$ynew <- (breakdown$Time)^{lambda_bc2}

par(mfrow=c(1,1))
plot(breakdown$ynew~breakdown$Voltage,ylab = 'Time transformed')
ggplot(data = breakdown, aes(x = Voltage, y = ynew))+
  geom_point()+
  geom_smooth(method = lm)

plot(breakdown$Time ~ breakdown$Voltage)

lm3_trans <- lm(ynew~Voltage, breakdown)
summary(lm3_trans)

par(mfrow=c(2,2))
plot(lm3_trans)

par(mfrow=c(1,1))
hist(resid(lm3_trans), breaks = 5)
shapiro.test(resid(lm3_trans))

#same stuff, different code
bc3 <- boxcox(lm3)
best_lam2 <- bc3$x[which.max(bc3$y)]
best_lam2

lm3.inverse <- lm((Time)^best_lam2 ~ Voltage, breakdown)
summary(lm3.inverse)
plot(lm3.inverse)

shapiro.test(resid(lm3.inverse))
shapiro.test(lm3.inverse$res)

