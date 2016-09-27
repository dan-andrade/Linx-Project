# straightforward kmeans

# from http://www.edureka.co/blog/clustering-on-bank-data-using-r/

library(dummies)
library(dplyr)
library(data.table)
library(cluster)
library(fpc)

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)

#small_[sapply(small_, function(x) nlevels(x)==2)] <- lapply(small_[sapply(small_, function(x) nlevels(x)==2)], as.numeric) 

small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))
smp_all_ <- dummy.data.frame(small_, names=names(small_)[c(2:4,6,9,13:20)])

vars <- names(smp_all_)
smp_all_[, vars] <- lapply(smp_all_[, vars], as.numeric) 
if (sum(is.na(smp_all_[vars]))) smp_all_[vars] <- na.roughfix(smp_all_[vars]) #impute by median/mode (randomForest)



#### special recoding: variables recoded from nr of levels (w/out dummies)
sk <- small_
sk2 <- sapply(sk, function(x) factor(x, labels=(1:length(levels(factor(x))))) )
sk3 = as.data.frame(sk2)
sk3[sapply(sk3, is.factor)] <- lapply(sk3[sapply(sk3, is.factor)],
                                      as.numeric) 

vars <- names(sk3)
if (sum(is.na(sk3[vars]))) sk3[vars] <- na.roughfix(sk3[vars]) #impute by median/mode (randomForest)

sk3_sc <- scale(sk3)
####



# elbom/scree plot
wss <- (nrow(smp_all_)-1)*sum(apply(smp_all_,2,var))
for(i in 2:15) {
  wss[i]<- sum(fit=kmeans(smp_all_,centers=i,15)$withinss)
}

plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")


fit <- kmeans(smp_all_,3)
fit
## checking withinss i.e. the intra cluster bond strength factor for each cluster
fit$withinss
## checking betweenss i.e. the inter cluster distance between cluster
fit$betweenss
fit$size   

# plot clusters
plotcluster(smp_all_,fit$cluster)
points(fit$centers,col=1:8,pch=16)
# clusplot(smp_all_, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


mydata <- data.frame(small_,fit$cluster)
head(mydata) # with cluster nr
# cluster means (works better for num vars):
cluster_mean.df <- data.frame(smp_all_[,c('seniority_yrs', 'experience_yrs', 'lang_english', 'lang_french', 'lang_spanish')],fit$cluster) 
cluster_mean <- aggregate(cluster_mean.df,by = list(fit$cluster),FUN = mean)
cluster_mean
