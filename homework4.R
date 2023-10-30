library(ggplot2)
library(tidyverse)
library(caret)
library(gvlma)
library(glmnet)
library(MASS)
library(stats)
library(boot)
library(lmtest)
library(plyr)
library(glmnet)

install.packages("leaps")
library(leaps)

#QUESTION 1
expand <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Expend.csv')
expand
head(expand)
dim(expand)

#a
ggplot(expand, aes(x = ECAB, y = EX))+
  geom_point()+
  ggtitle("EX vs ECAB")
#steep linear relationship, may be an influential point near (220, 450) that could cause the slope to change to polynomial

ggplot(expand, aes(x = MET, y = EX))+
  geom_point()+
  ggtitle("EX vs MET")
#no pattern

ggplot(expand, aes(x = GROW, y = EX))+
  geom_point()+
  ggtitle("EX vs GROW")
#somewhat of an increasing pattern, polynomial

ggplot(expand, aes(x = YOUNG, y = EX))+
  geom_point()+
  ggtitle("EX vs YOUNG")
#no apparent pattern, generally around 300

ggplot(expand, aes(x = OLD, y = EX))+
  geom_point()+
  ggtitle("EX vs OLD")
#no apparent pattern, generally around 300

ggplot(expand, aes(x = WEST, y = EX, group = WEST))+
  geom_boxplot()+
  ggtitle("EX vs WEST")
#West is much higher than non-west (25th percentile of West is greater than the median of non-west)

#b
#saturated model
expandlm1 <- lm(EX ~. - STATE, expand)
#saturated model did not work, not enough data for each individual state to create a decent model
summary(expandlm1)
gvlma(expandlm1)

#significant factors model 95%
expandlm2 <- lm(EX ~ ECAB + WEST, expand)
summary(expandlm2)

shapiro.test(resid(expandlm2))
bptest(expandlm2)
par(mfrow=c(2,2))
plot(expandlm2)
par(mfrow=c(1,1))
hist(resid(expandlm2), breaks = 15, xlim = c(-100,100))
gvlma(expandlm2)

#significant factors model 90%
expandlm3 <- lm(EX ~ ECAB + WEST + MET, expand)
summary(expandlm3)

shapiro.test(resid(expandlm3))
plot(expandlm3)
gvlma(expandlm3)

#polynomial model (^2) with 95% significant factors
expandlm4 <- lm(EX ~ poly(ECAB,2) + WEST, expand)
summary(expandlm4)

shapiro.test(resid(expandlm4))
plot(expandlm4)
gvlma(expandlm4)

#cross validation
expandtc <- trainControl(method = "cv", number = 5)

cvexpand1 <- train(EX ~ . - STATE, data=expand, method = 
                   "lm", trControl = expandtc, na.action = na.omit)
cvexpand1

cvexpand2 <- train(EX ~ ECAB + WEST, data=expand, method = 
                     "lm", trControl = expandtc, na.action = na.omit)
cvexpand2

cvexpand3 <- train(EX ~ ECAB + WEST + MET, data=expand, method = 
                     "lm", trControl = expandtc, na.action = na.omit)
cvexpand3

cvexpand4 <- train(EX ~ poly(ECAB,2) + WEST + MET, data=expand, method = 
                     "lm", trControl = expandtc, na.action = na.omit)
cvexpand4

#cp
fit1.full=regsubsets(EX~. - STATE, data =expand,nvmax = 6)
summary(fit1.full)

reg.summary=summary(fit1.full)
which.min(reg.summary$cp)

plot(reg.summary$cp,xlab="Number of Variables",
     ylab="Cp", type = "b",col="black",pch=17)

#R^2
plot(reg.summary$adjr2,xlab="Number of Variables",
     ylab="Adjusted RSq",
     type = "b",col="blue",pch=15)

plot(reg.summary$bic,xlab="Number of Variables",
     ylab="BIC", type = "b",col="black",pch=17)

#3
#Cook's Distance
cd <- cooks.distance(expandlm2)
plot(expandlm2, which = 4)
cd[47]
cd[42]
plot(cd)

#std resids
expandlm2_outliers = subset(expand,abs(stdres(expandlm2))>=3)
expandlm2_outliers
#no outliers

#leverage
expandlm2_leverage <- which(hat(model.matrix(expandlm2)) > 6/48)
expandlm2_leverage
#point 47, same one with high CD

all_expandlm2_leverage <- hat(model.matrix(expandlm2))
which(all_expandlm2_leverage > 6/48)
all_expandlm2_leverage[47]

plot(all_expandlm2_leverage,pch=18,col="black", main = "Leverage")
abline(h = 6/48)

expand[expandlm2_leverage > 6/48,c(1,3:4,6,7)]
a <- c(1,3:4,6,7)
a
#we can see that there is another point that is close to having high leverage

plot(expandlm2_leverage,pch=18,col="black")

#choose the other model
#Cook's Distance
cd <- cooks.distance(expandlm3)
plot(expandlm3, which = 4)
cd[47]
cd[42]

#std resids
expandlm3_outliers = subset(expand,abs(stdres(expandlm3))>=3)
expandlm3_outliers
hist(stdres(expandlm2), xlim = c(-4,4))
abline(v = -3)
abline(v = 3)


#no outliers

#leverage
expandlm3_leverage <- which(hat(model.matrix(expandlm3)) > 8/48)
expandlm3_leverage
#point 47 and 42, same ones with high CD

all_expandlm3_leverage <- hat(model.matrix(expandlm3))

plot(all_expandlm3_leverage,pch=18,col="black")
abline(h = 8/48)
#we can see that there is another point that is close to having high leverage

plot(expandlm3_leverage,pch=18,col="black")

#4
#remove point 47
new_expand <- expand[-c(47, 42),]
new_expand2 <- expand[-c(47),]

#fit new models
#previous best
new_expandlm2 <- lm(EX ~ ECAB + WEST, new_expand2)
summary(new_expandlm2)

par(mfrow=c(2,2))
plot(new_expandlm2)
par(mfrow=c(1,1))
hist(resid(new_expandlm2))
shapiro.test(resid(new_expandlm2))
bptest(new_expandlm2)
gvlma(new_expandlm2)

#test models
new_expandlm1 <- lm(EX ~. - STATE, new_expand)
summary(new_expandlm1)

new_expandlm3 <- lm(EX ~ ECAB + MET + GROW + WEST, new_expand)
summary(new_expandlm3)
#more variables tend to be significant in new model

new_expandlm4 <- lm(EX ~ poly(ECAB,2) + WEST + MET, data=new_expand)
summary(new_expandlm4)
#new polynomial is not significant

new_expandlm5 <- lm(EX ~ poly(ECAB,2) + WEST, data = new_expand)
summary(new_expandlm5)
#new polynomial is not significant

new_expandlm6 <- lm(EX ~ ECAB + WEST + MET, new_expand)
summary(new_expandlm6)

new_expandlm7 <- lm(EX ~ ECAB + WEST + GROW, new_expand)
summary(new_expandlm7)

#checking polynomial
set.seed(1)
cv.error=rep(0,10)
for(i in 1:10){
  glm.fit=glm(EX~poly(ECAB,i) + WEST + MET,data=new_expand)
  cv.error[i]=cv.glm(new_expand,glm.fit,K=10)$delta[1]
}

plot(cv.error, type ="b",xlab="degree of polynomial",col="black")
#no polynomial is likely needed

#cross validation on new data
new_expandtc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

new_cvexpand1 <- train(EX ~ . - STATE, data=new_expand2, method = 
                     "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand1

new_cvexpand2 <- train(EX ~ ECAB + WEST, data=new_expand2, method = 
                     "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand2

new_cvexpand3 <- train(EX ~ ECAB + WEST + MET, data=new_expand2, method = 
                     "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand3

new_cvexpand4 <- train(EX ~ poly(ECAB,2) + WEST + MET, data=new_expand2, method = 
                     "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand4

new_cvexpand5 <- train(EX ~ ECAB + WEST + MET + GROW, data=new_expand2, method = 
                         "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand5

new_cvexpand6 <- train(EX ~ poly(ECAB,2) + WEST + MET + GROW, data=new_expand2, method = 
                         "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand6

new_cvexpand7 <- train(EX ~ poly(ECAB,2) + WEST, data=new_expand2, method = 
                         "lm", trControl = new_expandtc, na.action = na.omit)
new_cvexpand7


#--- forget about this
#stepwise selection (AIC)
#forward
intercept_only <- lm(EX ~ 1, data=new_expand)
summary(intercept_only)

forward <- step(intercept_only, direction='forward', scope=(~. - STATE), trace=1, test = 'F')
forward
#giving an intercept only model, do not think this is accurate

#backwards
backwards <- step(new_expandlm1, direction='backward', trace=1, test = 'F')
backwards

#model contains ECAB, MET, GROW, and WEST
#----

#diagnostics
plot(new_expandlm2)
shapiro.test(resid(new_expandlm2))
gvlma(new_expandlm2)

plot(new_expandlm3)
shapiro.test(resid(new_expandlm3))
gvlma(new_expandlm3)

plot(new_expandlm4)
shapiro.test(resid(new_expandlm4))
gvlma(new_expandlm4)

plot(new_expandlm5)
shapiro.test(resid(new_expandlm5))
gvlma(new_expandlm5)

plot(new_expandlm6)
shapiro.test(resid(new_expandlm6))
gvlma(new_expandlm6)

plot(new_expandlm7)
shapiro.test(resid(new_expandlm7))
gvlma(new_expandlm7)

#best models
#model 2 - EX ~ ECAB + WEST
#model 3 - EX ~ ECAB + WEST + MET + GROW

#model 2
#Cook's Distance
cd2 <- cooks.distance(new_expandlm2)
plot(new_expandlm2, which = 4)
cd2[42]

#std resids
new_expandlm2_outliers = subset(new_expand2,abs(stdres(new_expandlm2))>=3)
new_expandlm2_outliers
hist(stdres(expandlm2), xlim = c(-4,4))
abline(v = -3)
abline(v = 3)

#no outliers

#leverage
new_expandlm2_leverage <- which(hat(model.matrix(new_expandlm2)) > 6/47)
new_expandlm2_leverage
#point 26 & 42

new_all_expandlm2_leverage <- hat(model.matrix(new_expandlm2))
new_all_expandlm2_leverage[26]
new_all_expandlm2_leverage[42]
6/47

plot(new_all_expandlm2_leverage,pch=18,col="black")
abline(h = 6/47)
#we can see that there is another point that is close to having high leverage

new_expand3 <- new_expand2[-c(26,42),]

third_iteration_expand <- lm(EX ~ ECAB + WEST, new_expand3)
summary(third_iteration_expand)

plot(third_iteration_expand)
shapiro.test(resid(third_iteration_expand))
bptest(third_iteration_expand)
gvlma(third_iteration_expand)

cd3 <- cooks.distance(third_iteration_expand)
plot(third_iteration_expand, which = 4)

#std resids
third_iteration_expand_outliers = subset(new_expand3,abs(stdres(third_iteration_expand))>=3)
third_iteration_expand_outliers
hist(stdres(expandlm2), xlim = c(-4,4))
abline(v = -3)
abline(v = 3)
#no outliers

#leverage
third_iteration_expand_leverage <- which(hat(model.matrix(third_iteration_expand)) > 6/45)
third_iteration_expand_leverage
#point 22

new_all_expandlm3_leverage <- hat(model.matrix(third_iteration_expand))

plot(new_all_expandlm2_leverage,pch=18,col="black")
abline(h = 6/45)

#---
new_expand4 <- new_expand3[-c(22),]

fourth_iteration_expand <- lm(EX ~ ECAB + WEST, new_expand4)
summary(fourth_iteration_expand)

plot(fourth_iteration_expand)
shapiro.test(resid(fourth_iteration_expand))
bptest(fourth_iteration_expand)
gvlma(fourth_iteration_expand)

cd3 <- cooks.distance(fourth_iteration_expand)
plot(fourth_iteration_expand, which = 4)

#std resids
fourth_iteration_expand_outliers = subset(new_expand4,abs(stdres(fourth_iteration_expand))>=3)
fourth_iteration_expand_outliers
#no outliers

#leverage
fourth_iteration_expand_leverage <- which(hat(model.matrix(fourth_iteration_expand)) > 6/44)
fourth_iteration_expand_leverage
#none

fourth_iteration_expand_leverage_shit <- hat(model.matrix(fourth_iteration_expand))

plot(fourth_iteration_expand_leverage_shit,pch=18,col="black", ylim = c(.03,.16))
abline(h = 6/44)









#model 3
#Cook's Distance
cd <- cooks.distance(new_expandlm3)
plot(new_expandlm3, which = 4)

#std resids
new_expandlm3_outliers = subset(expand,abs(stdres(new_expandlm3))>=3)
new_expandlm3_outliers

#no outliers

#leverage
new_expandlm3_leverage <- which(hat(model.matrix(new_expandlm3)) > 11/47)
new_expandlm3_leverage
#point 24, 39, & 42

new_all_expandlm3_leverage <- hat(model.matrix(new_expandlm3))

plot(new_all_expandlm3_leverage,pch=18,col="black")
abline(h = 11/47)

#model6
cd6 <- cooks.distance(new_expandlm6)
plot(new_expandlm6, which = 4)

#std resids
new_expandlm6_outliers = subset(expand,abs(stdres(new_expandlm6))>=3)
new_expandlm6_outliers
#no outliers

#leverage
new_expandlm6_leverage <- which(hat(model.matrix(new_expandlm6)) > 8/46)
new_expandlm6_leverage
#none

new_all_expandlm6_leverage <- hat(model.matrix(new_expandlm6))

plot(new_all_expandlm6_leverage,pch=18,col="black", ylim = c(0,.20))
abline(h = 8/46)

#e
newdata= data.frame(ECAB=(c(100,100,100,150,150,150,175,175,175)),
                    MET=(c(20,40,60,20,40,60,20,40,60)),
                    WEST=(c(0,0,0,0,0,0,0,0,0)))

round(predict(expandlm2, newdata),2)
round(predict(new_expandlm2, newdata),2)

#2
cereal <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/cereals.csv')
head(cereal)
dim(cereal)

#a
ggplot(cereal, aes(x = Type, Rating))+
  geom_boxplot()+
  ggtitle("Rating vs Type")
ddply(cereal,.(Type),nrow)
#only two cereal that were hot, not enough data

ggplot(cereal, aes(x = Manuf, Rating))+
  geom_boxplot()+
  ggtitle("Rating vs Manuf")
ddply(cereal,.(Manuf),nrow)

CalCor <- cor(cereal$Calories, cereal$Rating)
ProteinCor <- cor(cereal$Protein, cereal$Rating)
FatCor <- cor(cereal$Fat, cereal$Rating)
SodiumCor <- cor(cereal$Sodium, cereal$Rating)
FiberCor <- cor(cereal$Fiber, cereal$Rating)
CarboCor <- cor(cereal$Carbo, cereal$Rating)
SugarsCor <- cor(cereal$Sugars, cereal$Rating)
PotassCor <- cor(cereal$Potass, cereal$Rating,use = "complete.obs")
VitaminsCor <- cor(cereal$Vitamins, cereal$Rating)

correlations <- rbind(CalCor,ProteinCor,FatCor,SodiumCor,FiberCor,CarboCor,SugarsCor,PotassCor,VitaminsCor)
correlations

ggplot(cereal, aes(x= Sodium, y = Rating))+
  geom_point()+
  ggtitle("Rating vs Sodium")
ggplot(cereal, aes(x= Carbo, y = Rating))+
  geom_point()+
  ggtitle("Rating vs Carbo")
ggplot(cereal, aes(x= Sodium, y = Carbo))+
  geom_point()+
  ggtitle("Sodium vs Carbo")
ggplot(cereal, aes(x= Calories, y = Rating))+
  geom_point()
ggplot(cereal, aes(x= Sugars, y = Rating))+
  geom_point()

#b
cereallm <- lm(Rating ~. - Name, cereal)
summary(cereallm)

cereallm1 <- lm(Rating ~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, cereal)
coef(cereallm1)
summary(cereallm1)

simplecereallm1 <- lm(Rating ~ Calories + Protein + Fat  + Fiber + Carbo + Sugars + Potass + Vitamins, cereal)
summary(simplecereallm1)

par(mfrow=c(2,2))
plot(cereallm1)
par(mfrow=c(1,1))
hist(resid(cereallm1), breaks = 8)
shapiro.test(resid(cereallm1))
bptest(cereallm1)
gvlma(cereallm1)

fit1.full2=regsubsets(Rating~. - Name, data =cereal,nvmax = 11)
summary(fit1.full2)
reg.summary2=summary(fit1.full2)
which.min(reg.summary2$cp)

plot(reg.summary2$cp,xlab="Number of Variables",
     ylab="Cp", type = "b",col="black",pch=17)

plot(reg.summary2$adjr2,xlab="Number of Variables",
     ylab="Adjusted RSq",
     type = "b",col="black",pch=15)

plot(reg.summary2$bic,xlab="Number of Variables",
     ylab="BIC", type = "b",col="black",pch=1)


#stepwise selction

#backwards
backwards <- step(cereallm, direction='backward', trace=1, test = 'F')
backwards


#c
cerealcd <- cooks.distance(cereallm1)
plot(cereallm1, which = 4)
cerealcd[65]
cerealcd[4]


#std resids
cereallm1_outliers = subset(cereal,abs(stdres(cereallm1))>=3)
cereallm1_outliers
hist(stdres(cereallm1), xlim = c(-4,4))
abline(v = -3)
abline(v = 3)
#no outliers

#leverage
cereallm1_leverage <- which(hat(model.matrix(cereallm1)) > 20/76)
cereallm1_leverage

20/76
cereallm1_leverage_scores <- hat(model.matrix(cereallm1))
cereallm1_leverage_scores[4]
cereallm1_leverage_scores[67]
cereallm1_leverage_scores[65]
max(cereallm1_leverage_scores)
plot(cereallm1_leverage_scores,pch=18,col="black", main = "Leverage")
abline(h = 20/76)

#remove point 4
new_cereal <- cereal[-c(4),]

new_cereallm1 <- lm(Rating ~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, new_cereal)
summary(new_cereallm1)

shapiro.test(resid(new_cereallm1))
gvlma(new_cereallm1)
bptest(new_cereallm1)
plot(new_cereallm1)

#remove point 67
new_cereal2 <- cereal[-c(67),]

new2_cereallm1 <- lm(Rating ~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, new_cereal2)
new2_cereallm1

shapiro.test(resid(new2_cereallm1))
gvlma(new2_cereallm1)
bptest(new2_cereallm1)
par(mfrow=c(2,2))
plot(new2_cereallm1)
par(mfrow=c(1,1))
hist(resid(new2_cereallm1), breaks = 7)

#cook's
cerealcd2 <- cooks.distance(new2_cereallm1)
plot(new2_cereallm1, which = 4)
cerealcd2[4]


#std resids
cereallm3_outliers = subset(cereal,abs(stdres(new2_cereallm1))>=3)
cereallm3_outliers
hist(stdres(new2_cereallm1), xlim = c(-4,4))
abline(v = -3)
abline(v = 3)
#no outliers

#leverage
new2_cereallm1_leverage <- which(hat(model.matrix(new2_cereallm1)) > 20/75)
new2_cereallm1_leverage

20/75
new2_cereallm1_leverage_leverage_scores <- hat(model.matrix(new2_cereallm1))
plot(new2_cereallm1_leverage_leverage_scores,pch=18,col="black", main = "Leverage")
abline(h = 20/75)


#remove both
new_cereal3 <- cereal[-c(67,4),]

new3_cereallm1 <- lm(Rating ~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, new_cereal3)

shapiro.test(resid(new3_cereallm1))
gvlma(new3_cereallm1)
bptest(new3_cereallm1)
plot(new3_cereallm1)
hist(resid(new3_cereallm1), breaks = 10)

#d
cerealcvtrain <- trainControl(method = "LOOCV")

cerealcv <- train(Rating~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, data=cereal, method = 
                     "lm", trControl = cerealcvtrain, na.action = na.omit)
cerealcv

new_cerealcv <- train(Rating~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, data=new_cereal, method = 
                     "lm", trControl = cerealcvtrain, na.action = na.omit)
new_cerealcv

new_cerealcv2 <- train(Rating~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, data=new_cereal2, method = 
                        "lm", trControl = cerealcvtrain, na.action = na.omit)
new_cerealcv2

new_cerealcv3 <- train(Rating~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, data=new_cereal3, method = 
                         "lm", trControl = cerealcvtrain, na.action = na.omit)
new_cerealcv3

#RIDGE-LASSO
#no split
cereal_y <- cereal$Rating
cereal_x <- data.matrix(cereal[, c('Calories', 'Protein', 'Fat', 'Sodium', 'Fiber', 'Carbo', 'Sugars',
                                   'Potass', 'Vitamins')])
cereal_x_new <- cereal_x[complete.cases(cereal_x),]
cereal_y_new <- cereal_y[-c(5, 21)]


#LASSO REGRESSION
cereal_alpha1.fit <- cv.glmnet(cereal_x_new, cereal_y_new, type.measure="mse", 
                        alpha=1, family="gaussian")
coef(cereal_alpha1.fit)

plot(cereal_alpha1.fit)


#RIDGE REGRESSION
cereal_alpha0.fit <- cv.glmnet(cereal_x_new, cereal_y_new, type.measure="mse", 
                               alpha=0, family="gaussian")
coef(cereal_alpha0.fit)

#ELASTIC-NET


#just testing something
new_cerealx <- cereal[-c(67,4,12,43,23,1,2,5,8),]

new_cerealcvx <- train(Rating~ Calories + Protein + Fat + Sodium + Fiber + Carbo + Sugars + Potass + Vitamins, data=new_cerealx, method = 
                         "lm", trControl = cerealcvtrain, na.action = na.omit)
new_cerealcvx
