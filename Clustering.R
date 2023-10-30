library(ggplot2)
library(dplyr)


#k-Means
fifa19data <- read.csv('/Users/olinyoder/Desktop/Spring 2023/BSAN 450/Datasets/fifa19data.csv')
fifa19data<- fifa19data[complete.cases(fifa19data),]
t(fifa19data[1,])

ggplot(data=fifa19data)+geom_point(aes(x=GKDiving,y=GKReflexes),color='red')+
  xlab('GK Diving Score')+ylab('GK Reflex Score')

x<- select(fifa19data,c('GKDiving','GKReflexes'))
set.seed(1)
kmeans_k2<- kmeans(x,2,nstart = 20)
plot(x, col =(kmeans_k2$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =1)


#example 2
ggplot(data=fifa19data)+geom_point(aes(x=BallControl,y=Acceleration),color='blue')+
  xlab('Ball Control')+ylab('Acceleration')

x<- select(fifa19data,c('BallControl','Acceleration'))
set.seed(1)
kmeans_all<- sapply(2:10,function(i) kmeans(x, i,nstart = 20))
par(mfrow =c(1,2))

plot(2:10,sapply(1:9,function(i) kmeans_all[, i]$tot.withinss/kmeans_all[,i]$totss),
     xlab = 'Number of clusters K',ylab = "100*WSS/TSS",pch=20,cex=2,col="blue")

plot(2:10,sapply(1:9,function(i) min(kmeans_all[, i]$size)),
     xlab = 'Number of clusters K',ylab = "Minimum cluster size",pch=20,cex=2,col="red")

#gap statistic
library(cluster)
library(NbClust)

opt = clusGap(x, kmeans, K.max = 19, B = 2)
opt$Tab
plot(opt)
#use k = 3 clusters



#hierachical
set.seed(1)
idx<- sample(nrow(fifa19data),20)
hcdata<- fifa19data[idx,]
rownames(hcdata)<- fifa19data$Name[idx]
# Select `GKDiving` and `GKReflexes` from the data
library(dplyr)
x<- select(hcdata,c('GKDiving','GKReflexes'))

# Complete linkage
hc.complete =hclust(dist(x), method ="complete")
# Average linkage
hc.average =hclust(dist(x), method ="average")
# Single linkage
hc.single =hclust(dist(x), method ="single")

plot(hc.complete ,main ="Complete Linkage ", xlab="", sub ="",
     cex =.9)
plot(hc.average , main ="Average Linkage ", xlab="", sub ="",
     cex =.9)
plot(hc.single , main ="Average Linkage ", xlab="", sub ="",
     cex =.9)


hc.complete.clusters =cutree(hc.complete,4)
t(table(hc.complete.clusters ,rownames(x)))

xsc=scale (x)
plot(hclust(dist(xsc), method ="complete"), main ="Hierarchical Clustering with")

library(factoextra)

