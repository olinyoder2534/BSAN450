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
library(randomForest)

#QUESTION 1
highway <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/highway.csv')

#a
subset_highway <- regsubsets(Rate ~., highway, nvmax = 13)
subset_highway_summary <- summary(subset_highway)
subset_highway_summary

#using BIC
which.min(subset_highway_summary$bic)
#using mallows
which.min(subset_highway_summary$cp)
#using rss
which.min(subset_highway_summary$rss)
#using r^2
which.max(subset_highway_summary$adjr2)

#coefs
coef(subset_highway,5)


#b
highway_forward <- regsubsets(Rate ~., highway, nvmax = 13, method = 'forward')
#summary(highway_forward)

foward_highway_summary <- summary(highway_forward)

#using BIC
which.min(foward_highway_summary$bic)
#using mallows
which.min(foward_highway_summary$cp)
#using rss
which.min(foward_highway_summary$rss)
#using r^2
which.max(foward_highway_summary$adjr2)

#coefs
coef(highway_forward,7)

#c
highway_backwards <- regsubsets(Rate ~., highway, nvmax = 13, method = 'backward')
#summary(highway_backwards)

backwards_highway_summary <- summary(highway_backwards)

#using BIC
which.min(backwards_highway_summary$bic)
#using mallows
which.min(backwards_highway_summary$cp)
#using rss
which.min(backwards_highway_summary$rss)
#using r^2
which.max(backwards_highway_summary$adjr2)

coef(highway_backwards,5)

#d
train <- trainControl(method = "cv", number = 10)

subset_cv <- train(Rate ~ Acpt + Len + Slim + Sigs + Pa, data=highway, method = 
                     "lm", trControl = train, na.action = na.omit)
subset_cv
forward_cv <- train(Rate ~ Acpt + Len + Slim + Sigs + Pa + Trks + Fai, data=highway, method = 
                     "lm", trControl = train, na.action = na.omit)
forward_cv

backwards_cv <- train(Rate ~ Acpt + Len + Slim, data=highway, method = 
                     "lm", trControl = train, na.action = na.omit)
backwards_cv


#model diagnostics
highwaylm1 <- lm(Rate ~ Acpt + Len + Slim + Sigs + Pa, data=highway)
gvlma(highwaylm1)
shapiro.test(resid(highwaylm1))
bptest(highwaylm1)
#point 25 influential
plot(highwaylm1, which = 3)

highwaylm2 <- lm(Rate ~ Acpt + Len + Slim + Sigs + Pa + Trks + Fai, data=highway)
gvlma(highwaylm2)
shapiro.test(resid(highwaylm2))
bptest(highwaylm2)
#point 25 influential
plot(highwaylm2, which = 4)

highwaylm3 <- lm(Rate ~ Acpt + Len + Slim, data=highway)
gvlma(highwaylm3)
shapiro.test(resid(highwaylm3))
bptest(highwaylm3)
#point 25 influential
plot(highwaylm3, which = 4)
plot(highwaylm3)

#fit the model again without point 25
highway_new <- highway[-c(25),]

highwaylm1_new <- lm(Rate ~ Acpt + Len + Slim + Sigs + Pa, data=highway_new)
summary(highwaylm1_new)
gvlma(highwaylm1_new)
shapiro.test(resid(highwaylm1_new))
bptest(highwaylm1_new)
#point 25 influential
plot(highwaylm1_new, which = 3)
plot(highwaylm1_new)

highway_outliers = subset(highway_new,abs(stdres(highwaylm1_new))>=3)
highway_outliers
#no outliers

#leverage
highway_leverage <- which(hat(model.matrix(highwaylm1_new)) > 11/38)
highway_leverage

highway_leverage_points <- hat(model.matrix(highwaylm1_new))

plot(highway_leverage_points,pch=18,col="black")
abline(h = 11/38)



#PCA
pca <- prcomp(t(highway_new), scale=TRUE)

#base plots
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

#ggplot
pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text()+
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph")+
  geom_vline(xintercept=0, linetype="dashed", color = "black")+
  geom_hline(yintercept=.8, linetype="dashed", color = "black")

#------
loading_scores <- pca$rotation[,1]
scores <- abs(loading_scores) ## get the magnitudes
scores
scores_ranked <- sort(scores, decreasing=TRUE)
scores_ranked
top_scores <- names(scores_ranked[1:10])
top_scores


#QUESTION 2
cereals <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/cereals.csv')

#cleaning
sum(1*(is.na(cereals)))

cereals_new <- cereals[complete.cases(cereals),]

#a
subset_cereals <- regsubsets(Rating ~. - Name, cereals_new, nvmax = 11)#,really.big=T)
#Takes forever to compile
subset_cereals_summary <- summary(subset_cereals)
subset_cereals_summary

#using BIC
which.min(subset_cereals_summary$bic)
#using mallows
which.min(subset_cereals_summary$cp)
#using rss
which.min(subset_cereals_summary$rss)
#using r^2
which.max(subset_cereals_summary$adjr2)

coef(subset_cereals,9)


#-- 
coef.regsubsets <- function (object, id, vcov = FALSE, ...)
{
  s <- summary(object)
  invars <- s$which[id, , drop = FALSE]
  betas <- vector("list", length(id))
  for (i in 1:length(id)) {
    # added
    var.name <- names(which(invars[i, ]))
    thismodel <- which(object$xnames %in% var.name)
    names(thismodel) <- var.name
    # deleted
    #thismodel <- which(invars[i, ])
    qr <- .Fortran("REORDR", np = as.integer(object$np), 
                   nrbar = as.integer(object$nrbar), vorder = as.integer(object$vorder), 
                   d = as.double(object$d), rbar = as.double(object$rbar), 
                   thetab = as.double(object$thetab), rss = as.double(object$rss), 
                   tol = as.double(object$tol), list = as.integer(thismodel), 
                   n = as.integer(length(thismodel)), pos1 = 1L, ier = integer(1))
    beta <- .Fortran("REGCF", np = as.integer(qr$np), nrbar = as.integer(qr$nrbar), 
                     d = as.double(qr$d), rbar = as.double(qr$rbar), thetab = as.double(qr$thetab), 
                     tol = as.double(qr$tol), beta = numeric(length(thismodel)), 
                     nreq = as.integer(length(thismodel)), ier = numeric(1))$beta
    names(beta) <- object$xnames[qr$vorder[1:qr$n]] 
    reorder <- order(qr$vorder[1:qr$n])
    beta <- beta[reorder]
    if (vcov) {
      p <- length(thismodel)
      R <- diag(qr$np)
      R[row(R) > col(R)] <- qr$rbar
      R <- t(R)
      R <- sqrt(qr$d) * R
      R <- R[1:p, 1:p, drop = FALSE]
      R <- chol2inv(R)
      dimnames(R) <- list(object$xnames[qr$vorder[1:p]], 
                          object$xnames[qr$vorder[1:p]])
      V <- R * s$rss[id[i]]/(object$nn - p)
      V <- V[reorder, reorder]
      attr(beta, "vcov") <- V
    }
    betas[[i]] <- beta
  }
  if (length(id) == 1) 
    beta
  else betas
}

coef.regsubsets(subset_cereals, 9)
####

#b
forward_cereals <- regsubsets(Rating ~. - Name, cereals_new, nvmax = 11, method = 'forward')
forward_cereals_summary <- summary(forward_cereals)

#using BIC
which.min(forward_cereals_summary$bic)
#using mallows
which.min(forward_cereals_summary$cp)
#using rss
which.min(forward_cereals_summary$rss)
#using r^2
which.max(forward_cereals_summary$adjr2)

coef.regsubsets(forward_cereals, 9)


#c
backwards_cereals <- regsubsets(Rating ~. - Name, cereals_new, nvmax = 11, method = 'backward')
backwards_cereals_summary <- summary(backwards_cereals)

#using BIC
which.min(backwards_cereals_summary$bic)
#using mallows
which.min(backwards_cereals_summary$cp)
#using rss
which.min(backwards_cereals_summary$rss)
#using r^2
which.max(backwards_cereals_summary$adjr2)

coef.regsubsets(backwards_cereals, 9)


a#d
train2 <- trainControl(method = "cv", number = 5)

subset_cereals_cv <- train(Rating ~. - Name - Manuf - Type, data=cereals_new, method = 
                     "lm", trControl = train, na.action = na.omit)
subset_cereals_cv

cereals_cv2 <- train(Rating~Calories+Protein+Fat+Fiber+Sugars+Vitamins+Potass+Carbo,data=cereals_new, method = 
                      "lm", trControl = train, na.action = na.omit)
cereals_cv2

backwards_cereals_cv <- train(Rate ~ Acpt + Len + Slim, data=highway, method = 
                        "lm", trControl = train, na.action = na.omit)
backwards_cereals_cv

#other cv
set.seed(1)
glm.fit=glm(Rating~Calories+Protein+Fat+Fiber+Sugars+Vitamins+Potass+Carbo,data=cereals_new)
cv.error=cv.glm(cereals_new,glm.fit,K=nrow(cereals_new))$delta[1]
sqrt(cv.error)

#diagnostic
cerealslm1 <- lm(Rating ~. - Name - Manuf, cereals_new)
plot(cerealslm1, which = 4)
shapiro.test(resid(cerealslm1))
hist(resid(cerealslm1))
bptest(cerealslm1)
gvlma(cerealslm1)


#pca
cereals2 <- subset(cereals, select = -c(Name, Manuf, Type))
cereals_new2 <- cereals2[complete.cases(cereals2),]

pca2 <- prcomp(t(cereals2), scale=TRUE)

sapply(is.finite(cereals_new2))

