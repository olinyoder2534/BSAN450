library(tidyverse)
library(ggplot2)
library(caret)
library(tree)
library(e1071)
library(randomForest)
library(MASS)
library(stats)

install.packages("gbm")
library(gbm)

install.packages('ISLR')
library(ISLR)

seats <- Carseats

#QUESTION 1
sum(is.na(seats))
hist(seats$Sales)

#a
set.seed(3456)
trainIndex <- createDataPartition(seats$Sales, p = .5, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
seatsTrain <- seats[trainIndex,]
seatsTest  <- seats[-trainIndex,]

#b
regModel1 <- tree(Sales ~ ., seatsTrain)
summary(regModel1)
plot(regModel1)
text(regModel1, pretty = 1)

nodes <- as.numeric(rownames(regModel1$frame))
max(tree:::tree.depth(nodes))

#c
#selecting best size
regModel2=cv.tree(regModel1)
regModel2

plot(regModel2$size,regModel2$dev,type="b")

#pruning tree
regModel3 = prune.tree(regModel1, best=3)
summary(regModel3)
plot(regModel3)
text(regModel3, pretty = 1)

#making predictions
#pruned tree
pre1 <- predict(regModel3, seatsTest)
dcTreeMSE <- mean((pre1 - seatsTest$Sales)^2)
dcTreeMSE

#unpruned tree
pre2 <- predict(regModel1, seatsTest)
pruneddcTreeMSE <- mean((pre2 - seatsTest$Sales)^2)
pruneddcTreeMSE

#----
tr.control <- trainControl(method = "cv", number = 5)

tr = train(Sales ~., seatsTrain, method = "rpart", trControl = tr.control)
tr
bestTree <- tr$finalModel
summary(bestTree)
#----

#d
bagModel1 =randomForest(Sales~.,data=seatsTrain,mtry=10, importance =TRUE)
summary(bagModel1)
bagModel1
varImpPlot(bagModel1)

pre3 = predict(bagModel1 ,newdata =seatsTest)
baggedcTreeMSE <- mean((pre3 - seatsTest$Sales)^2)

#e
set.seed(1)
rfModel1 =randomForest(Sales~.,data=seatsTrain,mtry=3, importance =TRUE)
varImpPlot(rfModel1)
pre4 = predict(rfModel1,newdata =seatsTest)
rfMSE <- mean((pre4 - seatsTest$Sales)^2)

#f
set.seed(1)
boostModel1 =gbm(Sales~.,data=seatsTrain, distribution="gaussian",
                  n.trees=500, interaction.depth=4, shrinkage = .025)
best.iter2 <- gbm.perf(boostModel1, method = "OOB")
print(best.iter2)

pre5 = predict(boostModel1,newdata =seatsTest)
boostMSE <- mean((pre5 - seatsTest$Sales)^2)
boostMSE

#g
MSE <- data.frame(Method = c("DecisionTree", "PrunedDecisionTree", "Bagged", "RF", "Boosted"),
                             MSE = c(dcTreeMSE, pruneddcTreeMSE, baggedcTreeMSE, rfMSE, boostMSE))
MSE

#QUESTION 2
ames <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/ames.csv')
sum(is.na(ames))
hist(ames$Sale_Price)
ames<- ames[complete.cases(ames),]

#Convert all characters classes to factors
ames[sapply(ames, is.character)] <- lapply(ames[sapply(ames, is.character)], 
                                              as.factor)

#a
library(ISLR)
library(rpart)
library(rpart.plot)

set.seed(3456)
trainIndex <- createDataPartition(ames$Sale_Price, p = .5, 
                                  list = FALSE, 
                                  times = 1)
amesTrain <- ames[trainIndex,]
amesTest  <- ames[-trainIndex,]

#---train 2---
idx = sample(1:nrow(ames), ceiling(nrow(ames)/2))
ames.train = ames[idx,]
ames.test = ames[-idx,]
#----

#b
amesregModel1 <- tree(log(Sale_Price) ~ ., amesTrain)
summary(amesregModel1)
plot(amesregModel1)
text(amesregModel1, pretty = 1)

nodes2 <- as.numeric(rownames(amesregModel1$frame))
max(tree:::tree.depth(nodes2))

#c
#selecting best size
amesregModel2=cv.tree(amesregModel1)
amesregModel2

plot(amesregModel2$size,amesregModel2$dev,type="b")

#pruning tree
amesregModel3 = prune.tree(amesregModel1, best=5)
summary(amesregModel3)
plot(amesregModel3)
text(amesregModel3, pretty = 0)

#making predictions
#pruned tree
amespre1 <- predict(amesregModel3, amesTest)
amesdcTreeMSE <-mean((amespre1 - log(amesTest$Sale_Price))^2)

#unpruned tree
amespre2 <- predict(amesregModel1, amesTest)
amespruneddcTreeMSE <-mean((amespre2 - log(amesTest$Sale_Price))^2)

#d
amesbagModel1 =randomForest(log(Sale_Price)~.,data=amesTrain,mtry=80, importance =TRUE)
amesbagModel1
varImpPlot(amesbagModel1)

amespre3 = predict(amesbagModel1 ,newdata =amesTest)
amesbaggedcTreeMSE<- mean((amespre3 - log(amesTest$Sale_Price))^2)

#e
set.seed(1)
amesrfModel1 =randomForest(log(Sale_Price) ~. , data=amesTrain,mtry=9, importance =TRUE)
varImpPlot(amesrfModel1)
amespre4 = predict(amesrfModel1,newdata =amesTest)
amesrfMSE <- mean((amespre4 - log(amesTest$Sale_Price))^2)

#f
amesboostModel1 =gbm(log(Sale_Price)~.,data=amesTrain, distribution="gaussian",
                 n.trees=500, interaction.depth=4, shrinkage = .06)
best.iter <- gbm.perf(amesboostModel1, method = "OOB")
print(best.iter)

amespre5 = predict(amesboostModel1,newdata =amesTest)
amesboostMSE <- mean((amespre5 - log(amesTest$Sale_Price))^2)
amesboostMSE

amesMSE <- data.frame(Method = c("DecisionTree", "PrunedDecisionTree", "Bagged", "RF", "Boosted"),
                  MSE = c(amesdcTreeMSE, amespruneddcTreeMSE, amesbaggedcTreeMSE, amesrfMSE, amesboostMSE))
amesMSE
