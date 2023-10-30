library(ggplot2)
library(caret)
library(lmtest)
library(tidyverse)
library(gvlma)


# QUESTION 1
car <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/gasconsumption.csv')
head(car)
car2 <- car

#a
ggplot(car, aes(x = WT, y = GPM))+
  geom_point()+
  ggtitle('GPM vs Weight')

ggplot(car, aes(x = DIS, y = GPM))+
  geom_point()+
  ggtitle('GPM vs Displacement')

ggplot(car, aes(x = NC, y = GPM))+
  geom_point()+
  ggtitle('GPM vs Number of Cylinders')

#b
lm1 <- lm(GPM ~., car)
summary(lm1)

lm2 <-lm(GPM ~ WT + DIS, car)
summary(lm2)

#d
anova(lm2, lm1)

tc1 <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

cvlm1 <- train(GPM ~ ., data=car, method = 
                 "lm", trControl = tc1, na.action = na.omit)
cvlm1

cvlm2 <- train(GPM ~ WT + DIS, data=car, method = 
                 "lm", trControl = tc1, na.action = na.omit)
cvlm2

#e
par(mfrow=c(2,2))
plot(lm1)
par(mfrow=c(1,1))
hist(resid(lm1), breaks = 5)
shapiro.test(resid(lm1))
bptest(lm1)
gvlma(lm1)

standard_res <- as.data.frame(rstandard(lm1))
standard_res
which(standard_res > 3 | standard_res < -3)

#f
car2$NC <- as.factor(car2$NC)
lm_test <- lm(GPM ~ ., car2)
summary(lm_test)

# QUESTION 2
price <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/HousePrices.csv')
head(price)

#a
par(mfrow=c(2,2))
ggplot(price, aes(x = SqFt, y = Price))+
  geom_point()+
  ggtitle('Price vs Sq. Feet')

ggplot(price, aes(x = Bed, y = Price))+
  geom_point()+
  ggtitle('Price vs Bed')

ggplot(price, aes(x = Bath, y = Price))+
  geom_point()+
  ggtitle('Price vs Bath')

ggplot(price, aes(x = Offers, y = Price))+
  geom_point()+
  ggtitle('Price vs Offers')

#b
ggplot(price, aes(x = Brick, y = Price))+ #,color = Nbrhood))+
  geom_boxplot()+
  ggtitle('Price vs Brick')

ggplot(price, aes(x = Nbrhood, y = Price))+
  geom_boxplot()+
  ggtitle('Price vs Nbrhood')

ggplot(price, aes(x = Nbrhood, y = Price))+
  geom_violin()+
  ggtitle('Price vs Nbrhood')


#c
lm3 <- lm(Price ~., price)
summary(lm3)

plot(lm3)
par(mfrow=c(1,1))
hist(resid(lm3))
shapiro.test(resid(lm3))
bptest(lm3)
gvlma(lm3)

standard_res2 <- as.data.frame(rstandard(lm3))
standard_res2
which(standard_res2 > 3 | standard_res2 < -3)

# QUESTION 3
flowers <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/meadowform.csv')
head(flowers)

#a
plot(flowers$Intensity, flowers$Flowers)

ggplot(flowers, aes(x = Time, y = Flowers))+
  geom_boxplot()+
  ggtitle('Flowers vs Time')

lm4 <- lm(Flowers ~., flowers)
summary(lm4)
gvlma(lm4)

lm4.1 <- lm(Flowers ~ Time, flowers)
summary(lm4.1)

#b
group=ifelse(flowers$Time=="Early","E","L")
plot(flowers$Flowers~flowers$Intensity,pch=group)

#interaction
lm4inter <- lm(Flowers ~ Time + Intensity, flowers)
summary(lm4inter)




