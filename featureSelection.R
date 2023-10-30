library(leaps)

hitters <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/Hitters.csv')
head(hitters)

#new dataframe without name
hitters <- hitters[,-1]

#how many na's
sum(1*(is.na(hitters$Salary)))

#complete cases
hitters_new <- hitters[complete.cases(hitters),]

#best subset regression
best_subset_model <- regsubsets(Salary ~., hitters_new, nvmax = 19)
best_subset_model_summary <- summary(best_subset_model)

#best adj r^2
which.max(best_subset_model_summary$adjr2)
coef(best_subset_model, 11)

#smallest rss
which.min(best_subset_model_summary$rss)

#mallow's cp, measure of MSE on test set
which.min(best_subset_model_summary$cp)

#bic
which.min(best_subset_model_summary$bic)

#forwards stepwise
forward <- regsubsets(Salary ~., hitters_new, nvmax = 19, method = 'forward')
summary(forward)

#backwards stepwise
backwards <- regsubsets(Salary ~., hitters_new, nvmax = 19, method = 'backward')
summary(backwards)
#cannot do backwise when number of variables > num of sample size