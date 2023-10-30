library(ggplot2)
library(caret)
library(tidyverse)
library(MASS)
install.packages('gvlma')
library(gvlma)
library(boot)

#QUESTION 1
diamond <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Diamond.csv')
head(diamond)
dim(diamond)

#a
ggplot(diamond, aes(x = Carat, y = Price))+
  geom_point()
#when diamond is heavier, more heterogeneity in price. Meaning that we may need a non-linear transformation

ggplot(diamond, aes(x = Col, y = Price))+
  geom_boxplot()
tapply(diamond$Price, diamond$Col, mean)


ggplot(diamond, aes(x = Clar, y = Price))+
  geom_boxplot()
tapply(diamond$Price, diamond$Clar, mean)

ggplot(diamond, aes(x = CB, y = Price))+
  geom_boxplot()
tapply(diamond$Price, diamond$CB, mean)

#b
lm1 <- lm(Price ~., diamond)
summary(lm1)

plot(lm1, which = 1)
hist(resid(lm1))
shapiro.test(resid(lm1))
bptest(lm1)
gvlma(lm1)

#c
lm2 <- lm(Price ~ Carat + Col + Clar, diamond)
summary(lm2)

plot(lm2)
hist(resid(lm2))
shapiro.test(resid(lm2))
bptest(lm2)

#need a non-linear transformation

#bc <- boxcox(lm2)
#best_lam <- bc$x[which.max(bc$y)]
#best_lam

bc2 <- boxcox(lm1)
best_lam2 <- bc2$x[which.max(bc2$y)]
best_lam2

#lm2.inverse <- lm((Price)^best_lam ~ Carat + Col + Clar, diamond)
#summary(lm2.inverse)
#plot(lm2.inverse)
#shapiro.test(resid(lm2.inverse))
#bptest(lm2.inverse)
#gvlma(lm2.inverse)

#box-cox transformation functions
BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}

BCTransformInverse <- function(yt, lambda=0) {
  if (lambda == 0L) { exp(yt) }
  else { exp(log(1 + lambda * yt)/lambda) }
}

yt <- BCTransform(diamond$Price, best_lam2)
yo <- BCTransformInverse(yt, best_lam2)
unique(round(yo-diamond$Price),8)
#----------------

lm1.inverse <- lm((Price)^best_lam2 ~., diamond)
summary(lm1.inverse)
par(mfrow=c(1,1))
plot(lm1.inverse)
par(mfrow=c(1,1))
hist(resid(lm1.inverse))
plot(lm1.inverse, which = 4)
shapiro.test(resid(lm1.inverse))
bptest(lm1.inverse)
gvlma(lm1.inverse)

#outliers if larger or smaller the abs(3)

#standarized residuals
hist(stdres(lm1.inverse))

diamonds_2 = subset(diamond,abs(stdres(lm1.inverse))<=3)
diamonds_2
diamonds_2_outliers = subset(diamond,abs(stdres(lm1.inverse))>=3)
diamonds_2_outliers
#subset(diamonds, stdres(lm1.inverse) > 3)
#subset(diamonds, stdres(lm1.inverse) < -3)

#Cook's distance
cd <- cooks.distance(lm1.inverse)
plot(cd)
plot(lm1.inverse, which = 4)

subset(diamonds,abs(cooks.distance(lm1.inverse))>=.5)

#new model, model3
lm1.inverse2 = lm((Price)^best_lam2 ~ . - newPrice, diamonds_2)
summary(lm1.inverse2)
gvlma.lm(lm1.inverse2, alphalevel = 0.05)

#standarized residuals
hist(stdres(lm1.inverse2))

diamonds_3 = subset(diamonds_2,abs(stdres(lm1.inverse2))<=3)
diamonds_3
diamonds_3_outliers = subset(diamond,abs(stdres(lm1.inverse2))>=3)
diamonds_3_outliers
#subset(diamonds, stdres(lm1.inverse) > 3)
#subset(diamonds, stdres(lm1.inverse) < -3)

#Cook's distance
cd2 <- cooks.distance(lm1.inverse2)
plot(cd2)
plot(lm1.inverse2, which = 4)

#should consolidate with diamonds_3 subset
subset(diamonds,abs(cooks.distance(lm1.inverse2))>=.5)

#new model, model4
lm1.inverse3 <- lm((Price)^best_lam2 ~. - newPrice, diamonds_3)
summary(lm1.inverse3)

diamonds_4_outliers = subset(diamond,abs(stdres(lm1.inverse3))>=3)
diamonds_4_outliers

par(mfrow=c(1,1))
plot(lm1.inverse3)
hist(resid(lm1.inverse3))
shapiro.test(resid(lm1.inverse3))
bptest(lm1.inverse3)
hist(stdres(lm1.inverse3))
cd3 <- cooks.distance(lm1.inverse3)
plot(cd3)
gvlma.lm(lm1.inverse3, alphalevel = 0.5)
#since the p-value for hetero is < .05, significant at 95%, but not at 97.5%

#cv for all models
tc1 <- trainControl(method = "cv", number = 5)

cvdiamond1 <- train(Price ~ ., data=diamond, method = 
                      "lm", trControl = tc1, na.action = na.omit)
cvdiamond1

head(diamond)
cvdiamond2 <- train(Price ~ Carat + Col + Clar, data=diamond, method = 
                      "lm", trControl = tc1, na.action = na.omit)
cvdiamond2

#can't compare before and after transformation
diamond$newPrice <- diamond$Price^best_lam2
cvdiamond3 <- train(newPrice ~., data=diamond, method = 
                      "lm", trControl = tc1, na.action = na.omit)
cvdiamond3

cvdiamond4 <- train(newPrice ~. - newPrice, data=diamonds_2, method = 
                      "lm", trControl = tc1, na.action = na.omit)
cvdiamond4

cvdiamond5 <- train(newPrice ~. - newPrice, data=diamonds_3, method = 
                      "lm", trControl = tc1, na.action = na.omit)
cvdiamond5




#-----Don't worry about--------
standard_res <- as.data.frame(rstandard(lm1.inverse))
which(standard_res > 3 | standard_res < -3)

diamonds_new <- diamond[-c(116,120,131,279),]
head(diamonds_new)

ggplot(diamonds_new, aes(x = carat, y = price))+
  geom_point()

lm1.inverse_new <- lm((price)^best_lam2 ~. - Price, diamonds_new)
summary(lm1.inverse_new)
plot(lm1.inverse_new)
gvlma(lm1.inverse_new)
#------------

#QUESTION 2
corn <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/cornyield-1.csv')
head(corn)
dim(corn)

#a
ggplot(corn, aes(x = Rainfall, y = Yield))+
  geom_point()#+
  #geom_smooth()

ggplot(corn, aes(x = Year, y = Yield))+
  geom_point()

lmcorn1 <- lm(Yield ~ Rainfall + Year, corn)
summary(lmcorn1)

lmcorn2 <- lm(Yield ~ poly(Rainfall,2) + Year, corn)
summary(lmcorn2)

lmcorn3 <- lm(Yield ~ poly(Rainfall,3) + Year, corn)
summary(lmcorn3)

lmcorn4 <- lm(Yield ~ poly(Rainfall,4) + Year, corn)
summary(lmcorn4)

lmcorn5 <- lm(Yield ~ poly(Rainfall,5) + Year, corn)
summary(lmcorn5)

#---
lmcorn6 <- lm(Yield ~ poly(Rainfall,3) + poly(Year,1), corn)
summary(lmcorn6)

#---
lmcorn7 <- lm(Yield ~ poly(Year,2) + Rainfall, corn)
summary(lmcorn7)

lmcorn8 <- lm(Yield ~ poly(Year,3) + Rainfall, corn)
summary(lmcorn8)

lmcorn9 <- lm(Yield ~ poly(Year,4) + Rainfall, corn)
summary(lmcorn9)

lmcorn10 <- lm(Yield ~ poly(Year,5) + Rainfall, corn)
summary(lmcorn10)

#cross validation
tc <- trainControl(method = "repeatedcv", number = 5, repeats  = 3)

cvcorn1 <- train(Yield ~ Rainfall + Year, data=corn, method = 
                 "lm", trControl = tc, na.action = na.omit)
cvcorn1

cvcorn2 <- train(Yield ~ poly(Rainfall,2) + Year, data=corn, method = 
                 "lm", trControl = tc, na.action = na.omit)
cvcorn2

cvcorn3 <- train(Yield ~ poly(Rainfall,3) + Year, data=corn, method = 
                   "lm", trControl = tc, na.action = na.omit)
cvcorn3

cvcorn4 <- train(Yield ~ poly(Rainfall,4) + Year, data=corn, method = 
                   "lm", trControl = tc, na.action = na.omit)
cvcorn4

cvcorn5 <- train(Yield ~ poly(Rainfall,5) + Year, data=corn, method = 
                   "lm", trControl = tc, na.action = na.omit)
cvcorn5

cvcorn8 <- train(Yield ~ poly(Rainfall,8) + Year, data=corn, method = 
                   "lm", trControl = tc, na.action = na.omit)
cvcorn8

#plot
set.seed(1)
cv.error=rep(0,10)
for(i in 1:10){
  glm.fit=glm(Yield~poly(Rainfall,i) + Year,data=corn)
  cv.error[i]=cv.glm(corn,glm.fit,K=10)$delta[1]
}

plot(cv.error, type ="b",xlab="degree of polynomial",col="black")

#model diagnostics for 2nd degree
par(mfrow=c(2,2))
plot(lmcorn2)
par(mfrow=c(1,1))
hist(resid(lmcorn2), breaks = 8, xlim = c(-10,10))

shapiro.test(resid(lmcorn2))
bptest(lmcorn2)
gvlma(lmcorn2)

outliers <- subset(corn,abs(stdres(lmcorn2))>=3)
outliers

influencers <- subset(corn,abs(cooks.distance(lmcorn2))>=.5)
influencers


