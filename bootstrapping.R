library(ggplot2)
library(caret)
library(tidyverse)
library(MASS)
library(gvlma)
library(boot)

gpa <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/gpa.csv')
head(gpa)

cor(gpa$gpa, gpa$lsat)

ggplot(gpa, aes(x = lsat, y = gpa))+
  geom_point()+
  geom_smooth()

gpalm1 <- lm(gpa ~ lsat, gpa)
summary(gpalm1)

plot(gpalm1)
gvlma(gpalm1)


boot_func = function(gpa,index){
  return(coef(lm(gpa~lsat,data=gpa ,subset =index)))
}

boot_output=boot(gpa,boot_func,R=5000)
boot_output

boot_func2 = function(gpa,i){
  return(cor(gpa$gpa[i],gpa$lsat[i]))
}

boot_output2=boot(gpa,boot_func2,R=5000)
boot_output2


#insurance
a_hat = mean(log(X)) # ML estimate of mean parameter a
b_hat = sd(log(X)) # ML estimate of sd parameter b

a_hat = mean(log(X)) # ML estimate of mean parameter a
b_hat = sd(log(X)) # ML estimate of sd parameter b


mle_func = function(data,index){
  a = mean(log(X[index])) # ML estimate of mean parameter a
  b = sd(log(X[index]))
  theta_hat = 1-pnorm(log(10^6), mean = a, sd = b,
                      lower.tail = TRUE)
  return(theta_hat)
}
mle_func(X,1:100)

boot_output=boot(X,mle_func,R=5000)

lwr = quantile(boot_output$t,0.025)
upr = quantile(boot_output$t,0.975)
c(lwr,upr)