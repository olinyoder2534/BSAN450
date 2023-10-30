library(MASS)
library(ggplot2)

stop <- read.csv("/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/stopdistance.csv")
attach(stop)

plot(Stop.Dist~Speed)
ggplot(data = stop, aes(x = Speed, y = Stop.Dist))+
  geom_point()+
  geom_smooth()

#simple-linear model
lm.fit <- lm(Stop.Dist~Speed)
#print out the results
summary(lm.fit)

par(mfrow=c(1,1))
plot(lm.fit)
hist(resid(lm.fit))

shapiro.test(lm.fit$res)

cd <- cooks.distance(lm.fit)
cd[61]
cd[63]

#BOX-COX
bc<- boxcox(Stop.Dist~Speed)

#optimal lambda
notes_lambda_bc <- bc$x[which.max(bc$y)]
notes_lambda_bc

#transformation
stop$ynew <- (stop$Stop.Dist)^{notes_lambda_bc}

head(stop)

plot(stop$ynew ~ stop$Speed,ylab = 'Stop.Dist transformed')
plot(Stop.Dist~Speed)

#new model
lm.fit.trans <- lm(ynew~Speed, stop)
summary(lm.fit.trans)

#diagnostics
plot(lm.fit.trans)
hist(resid(lm.fit.trans), breaks = 8)

shapiro.test(resid(lm.fit.trans))

#predictions
newdata<- data.frame(Speed=c(5,10,15,20,25,30,35,40))
pred<- predict(lm.fit ,newdata,interval = "prediction",level=0.95)
pred.trans<- predict(lm.fit.trans ,newdata,interval = "prediction",level=0.95)

par(mfrow=c(1,2))
plot(newdata$Speed,pred[,1], xlab ="Speed",ylab="Stop.Dist",type="l",pch=17,col="blue")
points(newdata$Speed,pred[,2],type="l",col="red")
points(newdata$Speed,pred[,3],type="l",col="red")

#untransform data
plot(newdata$Speed,{pred.trans[,1]}^{1/lambda_bc},
     xlab ="Speed",ylab="Stop.Dist transformed",type="l",pch=17,col="blue")

points(newdata$Speed,{pred.trans[,2]}^{1/lambda_bc},
       type="l",col="red")

points(newdata$Speed,{pred.trans[,3]}^{1/lambda_bc},
       type="l",col="red")

ggplot(data = stop, aes(x = Speed, y = Stop.Dist))+
  geom_point()+
  geom_smooth()
