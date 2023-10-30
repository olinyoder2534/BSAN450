library(ISLR)
library(ggplot2)
library(factoextra)


nci.labs=NCI60$labs
nci.data=NCI60$data

#a
pr.out1 =prcomp(nci.data, scale=TRUE)
#pr.out1

#b
plot(pr.out1$x [,1:2], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z2")
plot(pr.out1$x[,c(1,3) ], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z3")

#c
pve1 =100* pr.out1$sdev ^2/ sum(pr.out1$sdev ^2)
plot(pve1 , type ="o", ylab="PVE ", xlab=" Principal Component ",
     col =" blue")
#use 8 pcas
plot(cumsum (pve1), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")
abline(v= 7)

pca.var1 <- pr.out1$sdev^2
pca.var.per1 <- round(pca.var1/sum(pca.var1)*100, 1)
barplot(pve1, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")


#d
nci.data1 <- scale(nci.data)
kmeans1 <- kmeans(nci.data1,4,nstart = 20)

kmeans1$size
100*kmeans1$betweenss/kmeans1$totss

fviz_cluster(kmeans1, data = nci.data1)
plot(nci.data1, col =(kmeans1$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =1)

