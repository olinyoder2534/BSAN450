
############ Homework 5 Problem 1 ################
hw5_data = read.csv("highway.csv")


##best subset regression
library(leaps)
bestsubset_reg = regsubsets(Rate~.,data=hw5_data,nvmax=13)
bestsubset_summary = summary(bestsubset_reg)
plot(1:13, bestsubset_summary$adjr2)
coef(bestsubset_reg,
     which.max(bestsubset_summary$adjr2))

bestsubset_model = lm(Rate~Len+Slim+Sigs+Acpt+Pa,
                      data=hw5_data)

#Forward stepwise regression
forward_reg = regsubsets(Rate~.,data=hw5_data,
                         nvmax=13,method="forward")
forward_summary = summary(forward_reg)
plot(1:13, forward_summary$adjr2)
coef(forward_reg,
     which.max(forward_summary$adjr2))

forward_model = lm(Rate~Len+Trks+Slim+Sigs+Acpt+Pa+Fai,
                      data=hw5_data)

#backward stepwise regression
backward_reg = regsubsets(Rate~.,data=hw5_data,
                         nvmax=13,method="backward")
backward_summary = summary(backward_reg)
plot(1:13, backward_summary$adjr2)
coef(backward_reg,
     which.max(backward_summary$adjr2))

backward_model = lm(Rate~Len+Slim+Sigs+Acpt+Pa,
                      data=hw5_data)

##### Cross Validation #####
library(boot)

bestsubset_glm = glm(Rate~Len+Slim+Sigs+Acpt+Pa,
                     data=hw5_data)
cv.error.bestsubsets=cv.glm(hw5_data,
                            bestsubset_glm,
                            K=10)$delta[1]

forward_glm = glm(Rate~Len+Trks+Slim+Sigs+Acpt+Pa+Fai,
                  data=hw5_data)
cv.error.forward=cv.glm(hw5_data,
                            forward_glm,
                            K=10)$delta[1]

backward_glm = glm(Rate~Len+Slim+Sigs+Acpt+Pa,
                     data=hw5_data)
cv.error.backward=cv.glm(hw5_data,
                            backward_glm,
                            K=10)$delta[1]
