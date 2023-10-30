library(ggplot2)

fifa <- read.csv("/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/fifa19data.csv")

# Getting rid of missing values
fifa <- fifa19data[complete.cases(fifa19data),]

head(fifa)

#random sample of 100 players
set.seed(100)
sample_players <- sample(1:nrow(fifa19data),100)
fifa19data_sample <- fifa19data[sample_players,]

head(fifa19data_sample)

#sprint speed based on acceleration
plot(SprintSpeed~Acceleration,data=fifa19data_sample,pch=3,col="red")

lm.fit <- lm(SprintSpeed~Acceleration,data=fifa19data_sample)
summary(lm.fit)

cd <- cooks.distance(lm.fit)
cd[1]

#plot of regression line)
plot(SprintSpeed~Acceleration,data=fifa19data_sample,pch=3,col="red")
abline(lm.fit,col="blue")

#gg-plot
ggplot(data = fifa19data_sample, aes(x = Acceleration, y = SprintSpeed))+
  geom_point()+
  geom_smooth(method = lm)

#confint
confint(lm.fit,level = 0.95)

#DIAGNOSTICS
lm.fit$res[1:5]

#resid vs fitted
par(mfrow=c(1,2))
plot(lm.fit$fitted.values,lm.fit$res)
plot(fifa19data_sample$Acceleration,lm.fit$res)

#hist and qq-plot
par(mfrow=c(1,2))
hist(lm.fit$res)
qqnorm(lm.fit$res)

par(mfrow=c(1,1))
plot(lm.fit)
hist(resid(lm.fit))

shapiro.test(lm.fit$res)
shapiro.test(resid(lm.fit))
#Since the p −value > 0.05, we fail to reject the null hypothesis that the residuals follow a Normal distribution.

#PREDICTIONS
set.seed(1)
idx<- sample(1:nrow(fifa19data[-c(sample_players),]),10,replace = F)
newplayers_Acceleration<- fifa19data[idx,'Acceleration']

# This is the true SprintSpeed score. We will not use this for prediction.
newplayers_SprintSpeed <- fifa19data[idx,'SprintSpeed']
newplayers_SprintSpeed

#prediction interval
pred_2 <- predict(lm.fit)
pred_2
pred_1<- predict(lm.fit, data.frame(Acceleration=newplayers_Acceleration),interval = "prediction",level=0.95)
cbind(data.frame(Acceleration=newplayers_Acceleration),pred_1)

#confidence interval
pred_2<- predict(lm.fit ,data.frame(Acceleration=newplayers_Acceleration),interval = "confidence",level=0.95)
cbind(data.frame(Acceleration=newplayers_Acceleration),pred_2)

#plots
newdata = data.frame(Acceleration=newplayers_Acceleration)
newdata$SprintSpeed_true<- newplayers_SprintSpeed
plot(newdata$Acceleration,pred_1[,1], xlab ="Acceleration",ylab="SprintSpeed",type="l",pch=17,col="blue",ylim=c(30,110))
points(newdata$Acceleration,pred_1[,2],type="l",col="red")
points(newdata$Acceleration,pred_1[,3],type="l",col="red")
# Plotting the True SprintSpeed score of the 10 new players in green triangles.
points(newdata$Acceleration,newdata$SprintSpeed_true,pch=17,col="green")

pred_3<- predict(lm.fit ,data.frame(Acceleration=newplayers_Acceleration),interval = "confidence",level=0.95)
cbind(data.frame(Acceleration=newplayers_Acceleration),pred_3)

newdata = data.frame(Acceleration=newplayers_Acceleration)
newdata$SprintSpeed_true<- newplayers_SprintSpeed
par(mfrow=c(1,2))
# Plotting the prediction intervals again (left plot)
plot(newdata$Acceleration,pred_1[,1], xlab ="Acceleration",ylab="SprintSpeed",type="l",pch=17,col="blue",ylim=c(30,110))
points(newdata$Acceleration,pred_1[,2],type="l",col="red")
points(newdata$Acceleration,pred_1[,3],type="l",col="red")
# Plotting the True SprintSpeed score of the 10 new players in green triangles.
points(newdata$Acceleration,newdata$SprintSpeed_true,pch=17,col="green")
# Plotting the prediction intervals and confidence intervals in the same plot (right plot)
plot(newdata$Acceleration,pred_1[,1], xlab ="Acceleration",ylab="SprintSpeed",type="l",pch=17,col="blue",ylim=c(30,110))
points(newdata$Acceleration,pred_1[,2],type="l",col="red")
points(newdata$Acceleration,pred_1[,3],type="l",col="red")
points(newdata$Acceleration,pred_2[,2],type="l",lty="dotted",col="black")
points(newdata$Acceleration,pred_2[,3],type="l",lty="dotted",col="black")
# Plotting the True SprintSpeed score of the 10 new players in green triangles.
points(newdata$Acceleration,newdata$SprintSpeed_true,pch=17,col="green")

