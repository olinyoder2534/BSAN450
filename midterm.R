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
library(leaps)
library(cape)

loss <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/loss.csv')
head(loss)
loss1 <- loss[-c(300),]

newData <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/newdata.csv')

#1 (just graphical, no box-cox)
ggplot(loss, aes(x = exret, y = loss))+
  geom_point()+
  ggtitle("Loss vs Extret")

ggplot(loss, aes(x = custcomplaint, y = loss))+
  geom_point()+
  ggtitle("Loss vs Custcomplaint")

ggplot(loss, aes(x = exdelay, y = loss))+
  geom_point()+
  ggtitle("Loss vs Exdelay")

ggplot(loss, aes(x = lowprior, y = loss))+
  geom_point()+
  ggtitle("Loss vs Lowprior")

ggplot(loss, aes(x = avglawsuits, y = loss))+
  geom_point()+
  ggtitle("Loss vs Avglawsuits")

model1 <- lm(loss ~., loss)
model2 <- lm(loss ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss1)

bc <- boxcox(model1)
best_la <- bc$x[which.max(bc$y)]
best_la

#take log transformation
log_loss <- log10(loss$loss)
sqrt_loss <- sqrt(loss$loss)

#graph again
ggplot(loss, aes(x = exret, y = sqrt_loss))+
  geom_point()+
  ggtitle("Loss vs Extret")

ggplot(loss, aes(x = custcomplaint, y = sqrt_loss))+
  geom_point()+
  ggtitle("Loss vs Customplaint")

ggplot(loss, aes(x = exdelay, y = sqrt_loss))+
  geom_point()+
  ggtitle("Loss vs Exdelay")

ggplot(loss, aes(x = lowprior, y = sqrt_loss))+
  geom_point()+
  ggtitle("Loss vs Lowprior")

ggplot(loss, aes(x = avglawsuits, y = sqrt_loss))+
  geom_point()+
  ggtitle("Loss vs Avglawsuits")

#2
subset_loss <- regsubsets(log_loss ~. - loss, loss, nvmax = 20)
subset_loss_sum <- summary(subset_loss)
subset_loss_sum

#cp
which.min(subset_loss_sum$cp)


plot(subset_loss_sum$cp,xlab="Number of Variables",
     ylab="CP", type = "b",col="black",pch=15)

coef(subset_loss,5)

loss_lm1 <- lm(log_loss ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss)
summary(loss_lm1)

#3 (outliers, redevelop model)

#basics
par(mfrow=c(1,1))
plot(loss_lm1)
hist(resid(loss_lm1))
shapiro.test(resid(loss_lm1))
bptest(loss_lm1)
gvlma(loss_lm1)

#Cooks
plot(loss_lm1, which = 4)
cd <- cooks.distance(loss_lm1)
cd[300]
cd[298]
cd[299]
cd[297]

#std resids
std_resid1 = subset(loss,abs(stdres(loss_lm1))>=3)
std_resid1

a <- stdres(loss_lm1)
a[298]
a[299]
a[300]
a[297]

hist(stdres(loss_lm1), breaks = 20, xlim = c(-10,10))
abline(v = -3)
abline(v = 3)

#leverage
high_leverage1 <- which(hat(model.matrix(loss_lm1)) > 11/300)
high_leverage1

high_leverage1_scores <- hat(model.matrix(loss_lm1))
plot(high_leverage1_scores,pch=18,col="black", main = "Leverage")
abline(h = 11/300)

#remove point 300 and 298
loss_new <- loss[-c(300, 298),]
log_loss_new <- log10(loss_new$loss)

loss_lm2 <- lm(log_loss_new ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss_new)

#basics
par(mfrow=c(2,2))
plot(loss_lm2)
hist(resid(loss_lm2))
shapiro.test(resid(loss_lm2))
bptest(loss_lm2)
gvlma(loss_lm2)

#Cooks
plot(loss_lm2, which = 4)
cd1 <- cooks.distance(loss_lm2)
cd1[297]

#std resids
std_resid2 = subset(loss_new,abs(stdres(loss_lm2))>=3)
std_resid2

b <- stdres(loss_lm2)
b[298]
b[297]

hist(stdres(loss_lm2), breaks = 20, xlim = c(-6,6))
abline(v = -3)
abline(v = 3)

#leverage
high_leverage2 <- which(hat(model.matrix(loss_lm2)) > 11/298)
high_leverage2

high_leverage2_scores <- hat(model.matrix(loss_lm2))
plot(high_leverage2_scores,pch=18,col="black", main = "Leverage")
abline(h = 11/298)

#remove points 299 and 297, but skeptical
loss_new2 <- loss[-c(300, 298, 299, 297),]
log_loss_new2 <- log10(loss_new2$loss)

loss_lm3 <- lm(log_loss_new2 ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss_new2)
summary(loss_lm3)

#basics
par(mfrow=c(2,2))
plot(loss_lm3)
hist(resid(loss_lm3), breaks = 8)
shapiro.test(resid(loss_lm3))
bptest(loss_lm3)
gvlma(loss_lm3)

#Cooks
plot(loss_lm3, which = 4)
cd2 <- cooks.distance(loss_lm3)
cd2[298]

#std resids
std_resid2 = subset(loss_new2,abs(stdres(loss_lm3))>=3)
std_resid2

c <- stdres(loss_lm3)

hist(stdres(loss_lm3), breaks = 7, xlim = c(-4,4))
abline(v = -3)
abline(v = 3)

#leverage
high_leverage3 <- which(hat(model.matrix(loss_lm3)) > 11/296)
high_leverage3

high_leverage3_scores <- hat(model.matrix(loss_lm3))
plot(high_leverage3_scores,pch=18,col="black", main = "Leverage")
abline(h = 11/296)

#4 (untransform data)
par(mfrow=c(1,1))

loss_lm3 <- lm(log_loss_new2 ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss_new2)

summary(loss_lm1)
summary(loss_lm3)


#------
modelA_pred <- 10^(predict(loss_lm1, newdata=newData))
modelA_pred

modelA_pred_pred <- 10^(predict(loss_lm1, newdata=newData, interval = "prediction"))
modelA_pred_pred

modelA_pred_conf <- 10^(predict(loss_lm1, newdata=newData, interval = "confidence"))
modelA_pred_conf

#------
modelB_pred <- 10^(predict(loss_lm3, newdata=newData))
modelB_pred

modelB_pred_pred <- 10^(predict(loss_lm3, newdata=newData, interval = "prediction"))
modelB_pred_pred

modelB_pred_conf <- 10^(predict(loss_lm3, newdata=newData, interval = "confidence"))
modelB_pred_conf

df2 <- data.frame(abs(modelA_pred_pred[,2] - modelA_pred_pred[,3]), abs(modelB_pred_pred[,2] - modelB_pred_pred[,3]))
colnames(df2)[1] ="ModelA Pred Interval Difference"
colnames(df2)[2] ="ModelB Pred Interval Difference"
kable(df2)

df3 <- data.frame(abs(modelA_pred_conf[,2] - modelA_pred_conf[,3]), abs(modelB_pred_conf[,2] - modelB_pred_conf[,3]))
colnames(df3)[1] ="ModelA Conf Interval Difference"
colnames(df3)[2] ="ModelB Conf Interval Difference"
kable(df3)




par(mfrow=c(1,1))


plot(modelA_pred, ylab = "Loss", ylim = c(0,70), main = "Predictions")
points(modelB_pred, col = 'red')
legend(12.5, 72, legend=c("Model A", "Model B"),
       col=c("black", "red"), pch = TRUE)

df <- data.frame(modelA_pred, modelB_pred)
df$difference <- (df$modelA_pred - df$modelB_pred)

kable(df)
#5
givenModel <- lm(log_loss ~ exret + custcomplaint + exdelay + lowprior, loss)
summary(givenModel)

givenModel2 <- lm(log10(loss) ~ exret + custcomplaint + exdelay + lowprior, loss)
summary(givenModel2)


#6
train <- trainControl(method = "cv", number = 10)
train2 <- trainControl(method = "LOOCV")

loss2 <- loss
loss2$log_loss <- log10(loss2$loss)

loss2_new <- loss2[-c(300, 298, 299, 297),]
loss2_new$log_loss <- log10(loss2_new$loss)

loss2_lm3 <- lm(log_loss ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, loss2_new)
summary(loss2_lm3)
summary(loss_lm3)

cv1 <- train(log_loss ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, data = loss2_new, method = 
               "lm", trControl = train, na.action = na.omit)
cv1

cv1.1 <- train(log10(loss) ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, data = loss_new2, method = 
               "lm", trControl = train, na.action = na.omit)
cv1.1

cv1.2 <- train(log10(loss) ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, data = loss[-c(297,298,299,300),], method = 
                 "lm", trControl = train, na.action = na.omit)
cv1.2

cv2 <- train(log_loss ~ exret + custcomplaint + exdelay + lowprior, loss2, method = 
               "lm", trControl = train, na.action = na.omit)
cv2

cv2.1 <- train(log10(loss) ~ exret + custcomplaint + exdelay + lowprior, loss, method = 
               "lm", trControl = train, na.action = na.omit)
cv2.1

cv2.1x <- train(log10(loss) ~ exret + custcomplaint + exdelay + lowprior, loss, method = 
                 "lm", trControl = train2, na.action = na.omit)
cv2.1x

#other model
set.seed(1)

glm.fit = glm(log10(loss) ~ exret + custcomplaint + exdelay + lowprior, data=loss)
cv.error = cv.glm(loss,glm.fit,K=10)$delta[1]
cv.error


glm.fit2 = glm(log10(loss) ~ exret + custcomplaint + exdelay + lowprior + avglawsuits, data=loss2_new)
cv.error2 = cv.glm(loss2_new,glm.fit,K=10)$delta[1]
cv.error2
