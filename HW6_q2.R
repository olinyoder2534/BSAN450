
ames = read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/ames.csv')
library(dplyr)
ames.new = mutate_if(ames, is.character, as.factor)

ames.new$logsale = log(ames.new$Sale_Price)

set.seed(1)
idx = sample(1:nrow(ames.new), ceiling(nrow(ames.new)/2))
ames.train = ames.new[idx,-79] #Removing Sale_Price
ames.test = ames.new[-idx,]

# Part b
library(tree)
tree.ames = tree(logsale~., data=ames.train)
summary(tree.ames)
plot(tree.ames)
text(tree.ames,pretty=0)

#Part c
set.seed(2)
cv.tree.ames=cv.tree(tree.ames) #SELECT BEST SIZE OF TREE  
cv.tree.ames # alpha is k 
plot(cv.tree.ames$size,cv.tree.ames$dev,type="b")

prune.cv.mytree = prune.tree(tree.ames, best=5) #choosing size 5
plot(prune.cv.mytree)
text(prune.cv.mytree)
title("Pruned tree with tree size tuned by CV!")

yhat <- predict(prune.cv.mytree, ames.test)
mse.pruned = mean((yhat - ames.test$logsale)^2)

yhat.unpruned <- predict(tree.ames, ames.test)
mse.unpruned = mean((yhat.unpruned - ames.test$logsale)^2)

mse.pruned
mse.unpruned

#d. bagged regression tree
library (randomForest)
set.seed (1)
bag.ames =randomForest(logsale~.,data=ames.train,mtry=80, importance =TRUE)
bag.ames

yhat.bag = predict(bag.ames ,newdata =ames.test)
mse.bagged = mean((yhat.bag-ames.test$logsale)^2)
mse.bagged

#e. Random Forest regression tree with `mtry=9`
set.seed (1)
rf.ames =randomForest(logsale~.,data=ames.train,mtry=9, importance =TRUE)
yhat.rf = predict(rf.ames ,newdata =ames.test)
mse.rf = mean((yhat.rf -ames.test$logsale)^2)
mse.rf

#f. boosted regression trees
library (gbm)
set.seed (1)
boost.ames =gbm(logsale~.,data=ames.train, distribution="gaussian",
                n.trees=500, interaction.depth=4)
yhat.boost=predict(boost.ames ,newdata=ames.test, n.trees=500)
mse.boost = mean((yhat.boost-ames.test$logsale)^2)
mse.boost
