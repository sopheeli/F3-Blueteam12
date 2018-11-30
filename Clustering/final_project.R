library(splines)
library(mclust)
library(factoextra)
library(dplyr)
#########################initial code#####################
times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data

betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}

cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

#########################initial code end#####################

#1a Perform a principal components analysis on columns 2 through 65. List the standard
#deviations for the first 5 components. 
pca <- princomp(cdata[,2:65])
pca$sdev[1:5]

#Comp.1    Comp.2    Comp.3    Comp.4    Comp.5
#45.21175 31.29008 22.37538 17.32995 13.04712

#b) Using all pca scores compute the optimal number of clusters using kmeans using both
#"wss" and the "silhouette" method. What is the optimal number of components using each 
#method. Why may this number be different?
fviz_nbclust(pca$scores, kmeans, method = "wss",k.max=20) #2 or 4
fviz_nbclust(pca$scores, kmeans, method = "silhouette",k.max=20) #2

#1c  Run the command "set.seed(12345)" and run a k-means clustering algorithm using the
#pca scores.
set.seed(12345)
k_means4 <- kmeans(pca$scores,4,nstart=25)
k_means4$centers
cdata$clust = k_means4$cluster

#1c a)Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph). 
clust_1 = cdata %>% filter(clust == 1)
clust_2 = cdata %>% filter(clust == 2)
clust_3 = cdata %>% filter(clust == 3)
clust_4 = cdata %>% filter(clust == 4)

cmeans_1 <- matrix(colMeans(clust_1[,6:65]),60,1)
cmeans_2 <- matrix(colMeans(clust_2[,6:65]),60,1)
cmeans_3 <- matrix(colMeans(clust_3[,6:65]),60,1)
cmeans_4 <- matrix(colMeans(clust_4[,6:65]),60,1)

View(head(cdata))

plot(times,X%*%cmeans_1, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%cmeans_2,lwd=2,col=2)
lines(times,X%*%cmeans_3,lwd=2,col=3)
lines(times,X%*%cmeans_4,lwd=2,col=4)
legend(2,80,legend = c("Cluster1","Cluster2","Cluster3","Cluster4"),
       col=c("black","red","green","blue"), lty =1,cex=1,y.intersp = 1)

#1c b)Look at cluster 3. Plot the graph of this cluster and give the mean values (on the original scale) for columns 2-65. 
#What makes this cluster different from the other clusters?  
#Describe this cluster so a physician can better understand important characteristics of these clusters. 
plot(times,X%*%cmeans_3, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
c1<- colMeans(clust_1[,2:65])
c2 <-colMeans(clust_2[,2:65])
c3 <-colMeans(clust_3[,2:65])
c4 <-colMeans(clust_4[,2:65])
c <- cbind(c1,c2,c3,c4)

#1c c)
sfun_1 <- splinefun(times,X%*%cmeans_1)
integrate(sfun_1,min(times),max(times)) #41.83153
sfun_2 <- splinefun(times,X%*%cmeans_2)
integrate(sfun_2,min(times),max(times)) #28.14693
sfun_3 <- splinefun(times,X%*%cmeans_3)
integrate(sfun_3,min(times),max(times)) #29.66037
sfun_4 <- splinefun(times,X%*%cmeans_4)
integrate(sfun_4,min(times),max(times)) #40.44697

#Part2######################################################
#2a Using mclustbic() and columns 10-20 of cdata (NOT the principal component values).
#estimate the optimal number of  cluster components using the BIC and only with 
#modelNames='VVV' and G = 1:20. Show a graph of the estimate. Is this number different than 
#the ones given above, why? (This will take a while). 
set.seed(12345)
cdata_bic <- mclustBIC(cdata[,10:20],G=1:20,modelNames = "VVV")
plot(cdata_bic)
summary(cdata_bic)

#2b Now using G = 6 and modelNames='VVV' and the same columns, 
#provide a graph of each cluster's mean curve (USING ALL OF THE DATA COLUMNS). 
#Put all plots on one graph. 
mclustF <- Mclust(cdata[,10:20],G=6,modelNames = "VVV")

summary(mclustF)
mclustF$classification

cdata$clust2 <- mclustF$classification

clust_1_bic = cdata %>% filter(clust2 == 1)
clust_2_bic = cdata %>% filter(clust2 == 2)
clust_3_bic = cdata %>% filter(clust2 == 3)
clust_4_bic = cdata %>% filter(clust2 == 4)
clust_5_bic = cdata %>% filter(clust2 == 5)
clust_6_bic = cdata %>% filter(clust2 == 6)

cmeans_1_bic <- matrix(colMeans(clust_1_bic[,6:65]),60,1)
cmeans_2_bic <- matrix(colMeans(clust_2_bic[,6:65]),60,1)
cmeans_3_bic <- matrix(colMeans(clust_3_bic[,6:65]),60,1)
cmeans_4_bic <- matrix(colMeans(clust_4_bic[,6:65]),60,1)
cmeans_5_bic <- matrix(colMeans(clust_5_bic[,6:65]),60,1)
cmeans_6_bic <- matrix(colMeans(clust_6_bic[,6:65]),60,1)

plot(times,X%*%cmeans_1_bic, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%cmeans_2_bic,lwd=2,col=2)
lines(times,X%*%cmeans_3_bic,lwd=2,col=3)
lines(times,X%*%cmeans_4_bic,lwd=2,col=4)
lines(times,X%*%cmeans_5_bic,lwd=2,col="pink")
lines(times,X%*%cmeans_6_bic,lwd=2,col="purple")
legend(2,80,legend = c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6"),
       col=c("black","red","green","blue","pink","purple"), lty =1,cex=1,y.intersp = 1)


#2c  Using all of the data compare cluster 4 with cluster 3 from the kmeans() cluster what can you 	
#say about the similarities between these two clusters, what are the differences? Which estimate 
#makes more sense? What do you trust more? What are the benefits of using mixture modeling over
#kmeans, what are the issues?

sfun_1_bic <- splinefun(times,X%*%cmeans_1_bic)
integrate(sfun_1_bic,min(times),max(times)) #29.58409
sfun_2_bic <- splinefun(times,X%*%cmeans_2_bic)
integrate(sfun_2_bic,min(times),max(times)) #39.15976
sfun_3_bic <- splinefun(times,X%*%cmeans_3_bic)
integrate(sfun_3_bic,min(times),max(times)) #36.03398
sfun_4_bic <- splinefun(times,X%*%cmeans_4_bic)
integrate(sfun_4_bic,min(times),max(times)) #29.27158
sfun_5_bic <- splinefun(times,X%*%cmeans_5_bic)
integrate(sfun_5_bic,min(times),max(times)) #31.18987
sfun_6_bic <- splinefun(times,X%*%cmeans_6_bic)
integrate(sfun_6_bic,min(times),max(times)) #38.66526


plot(times,X%*%cmeans_4_bic, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%cmeans_3,lwd=2,col=3)
legend(1.5,70,legend = c("Cluster3 K-means","Cluster4 BIC"),
       col=c("black","green"), lty =1,cex=1,y.intersp = 1)
c1<- colMeans(clust_1_bic[,2:65])
c2 <-colMeans(clust_2_bic[,2:65])
c3 <-colMeans(clust_3_bic[,2:65])
c4 <-colMeans(clust_4_bic[,2:65])
c5 <-colMeans(clust_5_bic[,2:65])
c6 <-colMeans(clust_6_bic[,2:65])
c <- cbind(c1,c2,c3,c4,c5,c6)

# plot the clusters together to find similar pairs
plot(times,X%*%cmeans_1, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%cmeans_2,lwd=2,col=2)
lines(times,X%*%cmeans_3,lwd=2,col=3)
lines(times,X%*%cmeans_4,lwd=2,col=4)
lines(times,X%*%cmeans_1_bic,lwd=2,col="pink")
lines(times,X%*%cmeans_2_bic,lwd=2,col="purple")
lines(times,X%*%cmeans_3_bic,lwd=2,col="yellow")
lines(times,X%*%cmeans_4_bic,lwd=2,col="orange")
lines(times,X%*%cmeans_5_bic,lwd=2,col="grey")
lines(times,X%*%cmeans_6_bic,lwd=2,col="brown")
legend(2,80,legend = c("Cluster1","Cluster2","Cluster3","Cluster4", 
                       "cluster_bic_1", "cluster_bic_2", "cluster_bic_3", "cluster_bic_4", "cluster_bic_5", "cluster_bic_6"),
       col=c("black","red","green","blue","pink","purple", "yellow", "orange", "grey", "brown"), lty =1,cex=1,y.intersp = 1)


plot(times,X%*%cmeans_2, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%cmeans_2_bic,lwd=2,col=2)
legend(2,80,legend = c("Cluster2 K-means", "Cluster2 BIC"),
       col=c("black","red"), lty =1,cex=1,y.intersp = 1)

