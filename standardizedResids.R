library(gvlma)

diamonds<- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Diamond.csv')
attach(diamonds)
par(mfrow=c(1,1))
plot(Price~Carat,col='red')
boxplot(Price~Col,col='green')
boxplot(Price~Clar,col='red')
boxplot(Price~CB,col='blue')

model_1 = lm(Price~.,data=diamonds)
summary(model_1)

plot(model_1$fitted.values,model_1$residuals)
plot(diamonds$Carat,model_1$residuals)
boxplot(model_1$residuals~diamonds$Col)

hist(model_1$residuals)
qqnorm(model_1$residuals)
shapiro.test(model_1$residuals)

library(MASS)
bc = boxcox(Price~.,data=diamonds)
lambda_bc = bc$x[which.max(bc$y)]
lambda_bc

diamonds$Price_new = (diamonds$Price)^{lambda_bc}

plot(diamonds$Carat,diamonds$Price_new)
plot(diamonds$Carat,diamonds$Price)

#model after only a box-cox transformation
model_2 = lm(Price_new~.-Price,data=diamonds)
summary(model_2)

par(mfrow=c(1,2))
plot(model_1$fitted.values,model_1$residuals)
plot(model_2$fitted.values,model_2$residuals)

shapiro.test(model_2$residuals)

#c
hist(stdres(model_2))

subset(diamonds,stdres(model_2)>3)
subset(diamonds,stdres(model_2)< -3)

cooks_influence = cooks.distance(model_2)
plot(cooks_influence)

diamonds_1 = subset(diamonds,abs(stdres(model_2))<=3)
diamonds_1

model_3 = lm(Price_new~.-Price,data=diamonds_1)
summary(model_3)

plot(model_3$fitted.values,model_3$residuals)

hist(stdres(model_3))
cooks_influence = cooks.distance(model_3)
plot(cooks_influence)

diamonds_2 = subset(diamonds_1,abs(stdres(model_3))<=3)

model_4 = lm(Price_new~.-Price,data=diamonds_2)
summary(model_4)

plot(model_4$fitted.values,model_4$residuals)

hist(stdres(model_4))
cooks_influence = cooks.distance(model_4)
plot(cooks_influence)
gvlma(model_4, alphalevel = 0.025)

