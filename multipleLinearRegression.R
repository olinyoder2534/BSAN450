install.packages('MLGdata')
library(MLGdata)
library(caret)

data("Abrasion")

head(Abrasion)

attach(Abrasion)
par(mfrow=c(1,2))
plot(Loss~Hard,pch=3,col="red")
plot(Loss~TS,pch=3,col="blue",xlab="TS: Tensile Strength")

#------
auto <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Auto.csv')
head(auto)

#in class method
set.seed(1)
train=sample(392,196)
test=(c(1:392))[-train]
fit1=lm(mpg~poly(horsepower,5), data=auto, subset=train)
summary(fit1)

pred=predict(fit1,auto)
mean((auto$mpg-pred)[test]^2)


#caret method
set.seed(3456)
trainIndex2 <- createDataPartition(auto$mpg, p = .8, 
                                  list = FALSE, 
                                  times = 1)
autoTrain <- auto[trainIndex2,]
autoTest  <- auto[-trainIndex2,]

fit1_2=lm(mpg~poly(horsepower,5), data=auto, subset=autoTrain)
summary(fit1_2)

pred2=predict(fit1_3,auto)
mean((auto$mpg-pred2)[autoTest]^2)


#leverage
head(mtcars)
model0 <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
summary(model0)

leverege <- hat(model.matrix(model0))
plot(leverege)
plot(model0, which = 4)

# number of predictors + 1/num observations
mtcars[leverege>.3,c(1,3:4,6,7)]
