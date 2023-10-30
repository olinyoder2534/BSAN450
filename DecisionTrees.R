#install.packages("ISLR")
#install.packages('tree')
library(ISLR)
library(tidyverse)
library(party)
library(tree)
library(rpart.plot)
library(caret)

# Remove NA data
hitters<- na.omit(Hitters)

#--------
sample_data = sample.split(hitters, SplitRatio = 0.7)
train_data <- subset(hitters, sample_data == TRUE)
test_data <- subset(hitters, sample_data == FALSE)

model <- tree(log_salary ~ . - Salary, train_data)
plot(model)
text(model)
rpart.plot(model)

predict_model <- predict(model, test_data)

ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  log(Salary) ~ .,
  data = train_data,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

bagged_cv
#-------

hitters$log_salary <- log10(hitters$Salary)

tree.hitters <- tree(log_salary ~ Years + Hits, hitters)
summary(tree.hitters)
plot(tree.hitters)
text(tree.hitters, pretty = 0)

#pruning
cv.tree.Hitters=cv.tree(tree.hitters) #SELECT BEST SIZE OF TREE
cv.tree.Hitters # alpha is k

#how many splits
cv.tree.Hitters$size[which.min(cv.tree.Hitters$dev)]
plot(cv.tree.Hitters$size,cv.tree.Hitters$dev,type="b")

prune.cv.mytree = prune.tree(tree.hitters, best=4)
plot(prune.cv.mytree)
text(prune.cv.mytree)

#train test split
idx = sample(1:nrow(Hitters), ceiling(nrow(Hitters)/2))
Hitters.train = Hitters[idx,]
Hitters.test = Hitters[-idx,]

tree.Hitters1=tree(log10(Salary)~., data=Hitters.train)
summary(tree.Hitters1)

plot(tree.Hitters1)
text(tree.Hitters1,pretty = 0)

#pruning
cv.tree.Hitters1=cv.tree(tree.Hitters1) #SELECT BEST SIZE OF TREE
cv.tree.Hitters1$size[which.min(cv.tree.Hitters1$dev)]
plot(cv.tree.Hitters1$size,cv.tree.Hitters1$dev,type="b")

prune.cv.mytree1 = prune.tree(tree.Hitters1, best=5)
plot(prune.cv.mytree1)
text(prune.cv.mytree1)

#predictions
yhat <- predict(prune.cv.mytree1, Hitters.test)
plot(yhat, log(Hitters.test$Salary))

mean((yhat - log(Hitters.test$Salary))^2)

#CLASSIFICATION TREES
Heart <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Heart.csv')

Heart <- na.omit(Heart)

idx = sample(1:nrow(Heart), ceiling(nrow(Heart)/2))
Heart.train = Heart[idx,]
Heart.test = Heart[-idx,]
tree.Heart=tree(as.factor(AHD)~., data=Heart.train)
summary(tree.Heart)

plot(tree.Heart)
text(tree.Heart, pretty=0)

cv.tree.Heart=cv.tree(tree.Heart,FUN=prune.misclass,K=3) #SELECT BEST SIZE OF TREE
cv.tree.Heart$size
cv.tree.Heart$dev
plot(cv.tree.Heart$size, cv.tree.Heart$dev)
plot(cv.tree.Heart)

prune.cv.mytree = prune.tree(tree.Heart, best=6,method = 'misclass')
plot(prune.cv.mytree)
text(prune.cv.mytree, pretty=0)

tree.pred <- predict(prune.cv.mytree, Heart.test, type='class')
confusionMatrix(tree.pred, as.factor(Heart.test$AHD))
