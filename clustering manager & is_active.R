library(calibrate)
library(dummies)
library(dplyr)
library(doBy)
library(reshape2)


mngr_active <- table(all.small$manager_name, all.small$is_active)


mngr_active <- as.data.frame(mngr_active)
mngr_active.wide <- dcast(mngr_active, Var1 ~ Var2, value.var = 'Freq')
mngr_active.wide$NO_perc <- round(mngr_active.wide$NO/(mngr_active.wide$NO+mngr_active.wide$YES), 2)
mngr_active.wide$YES_perc <- round(mngr_active.wide$YES/(mngr_active.wide$NO+mngr_active.wide$YES), 2)
mngr_active.wide <- orderBy(~-NO_perc, mngr_active.wide)
write.csv2(mngr_active.wide, file='c:/users/altran/documents/linx project/mngr_active.wide.csv')


plot(mngr_active.wide$NO, mngr_active.wide$YES)
textxy(mngr_active.wide$NO, mngr_active.wide$YES, mngr_active.wide$NO_perc)
abline(lsfit(mngr_active.wide$NO, mngr_active.wide$YES))


###

mngr_active.wide_small <- mngr_active.wide[2:32, 1:3]

mng.cl <- dummy.data.frame(mngr_active.wide_small, names=names(mngr_active.wide_small)[1])

mng.cl.sc <- scale(mng.cl)

wss <- (nrow(mng.cl.sc)-1)*sum(apply(mng.cl.sc,2,var))
for(i in 2:15) {
  wss[i]<- sum(fit=kmeans(mng.cl.sc,centers=i,15)$withinss)
}

plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")

k <- 4
fit <- kmeans(mng.cl.sc, k)
fit
## checking withinss i.e. the intra cluster bond strength factor for each cluster
fit$withinss
## checking betweenss i.e. the inter cluster distance between cluster
fit$betweenss
fit$size   

# plot clusters
plotcluster(mng.cl.sc,fit$cluster)
# points(fit$centers,col=1:8,pch=16)
# clusplot(mng.cl.sc, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


mydata <- data.frame(mngr_active.wide[2:32, ],fit$cluster)
head(mydata) # with cluster nr
# cluster means (works better for num vars):
cluster_mean.df <- data.frame(mngr_active.wide[2:32,c('NO', 'YES', 'NO_perc', 'YES_perc')],fit$cluster) 
cluster_mean <- aggregate(cluster_mean.df,by = list(fit$cluster),FUN = mean)
cluster_mean

boxplot(mydata$YES ~ mydata$fit.cluster)
boxplot(mydata$NO ~ mydata$fit.cluster)
orderBy(~-fit.cluster, mydata)

plot(mydata$NO, mydata$YES)
textxy(mydata$NO, mydata$YES, mydata$fit.cluster)
abline(lsfit(mydata$NO, mydata$YES))

plot(mngr_active.wide$NO, mngr_active.wide$YES)
textxy(mngr_active.wide$NO, mngr_active.wide$YES, mngr_active.wide$NO_perc)
abline(lsfit(mngr_active.wide$NO, mngr_active.wide$YES))

plot(mngr_active.wide$NO, mngr_active.wide$YES)
textxy(mngr_active.wide$NO, mngr_active.wide$YES, mngr_active.wide$Var1)
abline(lsfit(mngr_active.wide$NO, mngr_active.wide$YES))
