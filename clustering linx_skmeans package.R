# http://workinganalytics.com/data-smart-ch2-customer-segmentation-with-r-using-k-medians-clustering-2/

library(skmeans)
library(cluster)
library(dummies)
library(dplyr)
library(data.table)
library(doBy)
library(randomForest)

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)

#small_[sapply(small_, function(x) nlevels(x)==2)] <- lapply(small_[sapply(small_, function(x) nlevels(x)==2)], as.numeric) 

small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))
smp_all_ <- dummy.data.frame(small_, names=names(small_)[c(2:4,6,9,13:20)])

vars <- names(smp_all_)
smp_all_[, vars] <- lapply(smp_all_[, vars], as.numeric) 
if (sum(is.na(smp_all_[vars]))) smp_all_[vars] <- na.roughfix(smp_all_[vars]) #impute by median/mode (randomForest)

matrix_all.main <- as.matrix(smp_all_)

# Segment the observations into k clusters
partition <- skmeans(matrix_all.main, 6)

# Look at the segmentation outcome summary
partition # returns a summary statement for the process
partition$cluster # returns a vector showing cluster assignment for each row

# Create a vector of customer names for each cluster
cluster_1 <- names(partition$cluster[partition$cluster == 1])
cluster_2 <- names(partition$cluster[partition$cluster == 2])
cluster_3 <- names(partition$cluster[partition$cluster == 3])
cluster_4 <- names(partition$cluster[partition$cluster == 4])
cluster_5 <- names(partition$cluster[partition$cluster == 5])
cluster_6 <- names(partition$cluster[partition$cluster == 6])

# Examine one of the clusters, as an example
cluster_1

# Compare performance of the k=6 clustering process with the k=5 clustering using the silhouette function. The closer the value is to 1, the better
silhouette_k6 <- silhouette(partition)
summary(silhouette_k6)  
plot(silhouette_k6)

partition_k5 <- skmeans(matrix_all.main, 5)
silhouette_k5 <- silhouette(partition_k5)
plot(silhouette_k5) 
summary(silhouette_k5)

# cluster in df
cluster.df <- as.data.frame(partition$cluster)
colnames(cluster.df)[1] <- 'cluster'
cluster.df$row_nr <- rownames(cluster.df)

# join linx ds with clusters
linx.ds <- small_
linx.ds[10:12] <- sapply(linx.ds[10:12], as.numeric)
linx.clusters <- cbind(linx.ds, cluster.df$cluster)
setnames(linx.clusters, old='cluster.df$cluster', new='cluster')
# order by cluster
linx.clusters <- orderBy(~cluster, linx.clusters)
write.csv2(linx.clusters, file='C:/Users/Altran/Desktop/BD/29-08/R output/linx.clusters.csv')

pie(colSums(matrix_all.main[partition$cluster==1,]),cex=0.5)
pie(colSums(matrix_all.main[partition$cluster==2,]),cex=0.5)
pie(colSums(matrix_all.main[partition$cluster==3,]),cex=0.5)
pie(colSums(matrix_all.main[partition$cluster==4,]),cex=0.5)
pie(colSums(matrix_all.main[partition$cluster==5,]),cex=0.5)
pie(colSums(matrix_all.main[partition$cluster==6,]),cex=0.5)


cluster1.linx <- linx.clusters %>% filter(cluster==1)
cluster2.linx <- linx.clusters %>% filter(cluster==2)
cluster3.linx <- linx.clusters %>% filter(cluster==3)
cluster4.linx <- linx.clusters %>% filter(cluster==4)
cluster5.linx <- linx.clusters %>% filter(cluster==5)
cluster6.linx <- linx.clusters %>% filter(cluster==6)

#hist of experience_yrs
par(mfrow=c(3,3))
hist(cluster1.linx$experience_yrs)
hist(cluster2.linx$experience_yrs)
hist(cluster3.linx$experience_yrs)
hist(cluster4.linx$experience_yrs)
hist(cluster5.linx$experience_yrs)
hist(cluster6.linx$experience_yrs)

#hist of seniority_yrs
par(mfrow=c(3,3))
hist(cluster1.linx$seniority_yrs)
hist(cluster2.linx$seniority_yrs)
hist(cluster3.linx$seniority_yrs)
hist(cluster4.linx$seniority_yrs)
hist(cluster5.linx$seniority_yrs)
hist(cluster6.linx$seniority_yrs)

# positions
# comparison between clusters 4 and 5
dfpos4 <- cluster4.linx %>% group_by(position) %>% summarise(count=n()) 
dfpos4$position <- reorder(dfpos4$position, dfpos4$count)

ggplot(dfpos4, aes(y=count)) +
  geom_bar(data=dfpos4, aes(x=position, fill=position), stat="identity") +
  ggtitle("Positions Cluster 4") 

dfpos5 <- cluster5.linx %>% group_by(position) %>% summarise(count=n()) 
dfpos5$position <- reorder(dfpos5$position, dfpos5$count)

ggplot(dfpos5, aes(y=count)) +
  geom_bar(data=dfpos5, aes(x=position, fill=position), stat="identity") +
  ggtitle("Positions Cluster 5") 


# is_active
active <- linx.clusters %>% group_by(is_active, cluster) %>% summarise(count=n())
active2$perc <- ifelse(active2$is_active=='NO',
                       active2$count/nrow(filter(linx.clusters, is_active=='NO')),
                                                 active2$count/nrow(filter(linx.clusters, is_active=='YES')))

active.wide <- dcast(linx.clusters, cluster ~ is_active)
active.wide$NO_perc <- paste(round(active.wide$NO/(active.wide$NO+active.wide$YES)*100, 1), '%')
active.wide$YES_perc <- paste(round(active.wide$YES/(active.wide$NO+active.wide$YES)*100, 1), '%')


# overall level of english by experience_yrs
boxplot(linx.clusters$experience_yrs ~ linx.clusters$lang_english)
# level of english by experience_yrs by cluster
boxplot(cluster1.linx$experience_yrs ~ cluster1.linx$lang_english)
boxplot(cluster2.linx$experience_yrs ~ cluster2.linx$lang_english)
boxplot(cluster3.linx$experience_yrs ~ cluster3.linx$lang_english)
boxplot(cluster4.linx$experience_yrs ~ cluster4.linx$lang_english)
boxplot(cluster5.linx$experience_yrs ~ cluster5.linx$lang_english)
boxplot(cluster6.linx$experience_yrs ~ cluster6.linx$lang_english)



#hist of lang_english

par(mfrow=c(3,3))
hist(cluster1.linx$lang_english)
hist(cluster2.linx$lang_english)
hist(cluster3.linx$lang_english)
hist(cluster4.linx$lang_english)
hist(cluster5.linx$lang_english)
hist(cluster6.linx$lang_english)
par(mfrow=c(1,1))


# contract_type
barplot(table(linx.clusters$contract_type, linx.clusters$cluster))
ggplot(linx.clusters, aes(cluster, fill=contract_type)) + geom_bar() +
  ggtitle("contract_type by cluster")

###
# elbom/scree plot
wss <- (nrow(smp_all_)-1)*sum(apply(smp_all_,2,var))
for(i in 2:15) {
  wss[i]<- sum(fit=kmeans(smp_all_,centers=i,15)$withinss)
}

plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")
