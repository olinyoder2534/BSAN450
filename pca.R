


url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)
dim(my_basket)

head(my_basket[,1:10])

x<-my_basket
pr.out =prcomp(x, scale=TRUE)

plot(1:42,pr.out$rotation[,1],pch=17,col="red",xaxt="n",
     xlab = 'Loading of first PC')
axis(1, at=1:42, labels=names(x))

plot(1:42,pr.out$rotation[,2],pch=15,col="blue",xaxt="n",
     xlab = 'Loading of second PC')
axis(1, at=1:42, labels=names(x))

idx = order(pr.out$rotation[,1])
loading1_ordered = pr.out$rotation[idx,1]
plot(loading1_ordered,1:42,pch=17,col="red",yaxt="n",
     xlab = 'Ordered Loading of first PC',ylab='')
axis(2, at=1:42, labels=names(x)[idx],las=2)

plot(pr.out$rotation[,1],pr.out$rotation[,2],col='red',pch=17)
text(pr.out$rotation[,2]~pr.out$rotation[,1], labels=names(x),
     data=x, cex=0.9, font=1)
abline(h=0,col="gray50",lty=2)

abline(v=0,col="gray50",lty=2)

pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
     col =" blue")
#use 8 pcas
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")


#----PCR
install.packages('pls')
library(pls)

fit_pcr = pcr(whiskey~., data = my_basket, scale = T, validation = c("CV"))
summary(fit_pcr)

validationplot(fit_pcr, val_type = "MSEP")

#predictions
train = my_basket[1:1000,]
test = my_basket[1001:2000,]
preds = predict(fit_pcr, new_data = test, ncomps = 7)

coefplot(fit_pcr, ncomp = 7, label = colnames(x), pretty.xlabels = F)
a <- fit_pcr$coefficients[,,7]
a
