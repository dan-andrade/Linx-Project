library(randomForest)
library(dummies)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)

small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))
smp_all_ <- dummy.data.frame(small_, names=names(small_)[c(2:4,6,9,13:20)])

vars <- names(smp_all_)
smp_all_[, vars] <- lapply(smp_all_[, vars], as.numeric) 
if (sum(is.na(smp_all_[vars]))) smp_all_[vars] <- na.roughfix(smp_all_[vars]) #impute by median/mode (randomForest)

pca <- princomp(smp_all_)
plot(pca)
plot(pca, type='l')
summary(pca)
loadings(pca)

### in case of need for scaling:
# Scale
data2 <- data.frame(scale(smp_all_))
# Verify variance is uniform
plot(sapply(data2, var))
# Proceed with principal components
pc <- princomp(data2)
plot(pc)
plot(pca, type='l')
summary(pc)
###


# with prcomp

#Get principal component vectors using prcomp instead of princomp
prc <- prcomp(smp_all_)
plot(prc)
plot(prc, type='l')
summary(prc)
loadings(prc)

# Eigenvalues
eig <- (prc$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.decathlon2.active <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
head(eig.decathlon2.active)


# scree plot

library("factoextra")
fviz_screeplot(prc, ncp=10)
# with eigenvalues
fviz_screeplot(prc, ncp=10, choice="eigenvalue")
# criterium of nr of eigenvalues: >1 1 indicates that PCs account for 
# more variance than accounted by one of the original variables in standardized data 
# This is commonly used as a cutoff point for which PCs are retained
eig.val <- get_eigenvalue(prc)
head(eig.val)


# First four principal components
comp <- data.frame(prc$x[,1:4])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)

# Determine number of clusters
wss <- (nrow(smp_all_)-1)*sum(apply(smp_all_,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(smp_all_,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 3
# Apply k-means with k=3
k <- kmeans(comp, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)


# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))


# First cluster
row.names(smp_all_[k$clust==clust[1],])
# Second Cluster
row.names(smp_all_[k$clust==clust[2],])
# Third Cluster
row.names(smp_all_[k$clust==clust[3],])


#boxplots

boxplot(small_$experience_yrs ~ k$cluster,
        xlab='Cluster', ylab='Experience in years',
        main='Experience by Cluster')
boxplot(small_$seniority_yrs ~ k$cluster,
        xlab='Cluster', ylab='Altran seniorirty in years',
        main='Seniority by Cluster')

boxplot(as.numeric(small_$lang_english) ~ k$cluster)
boxplot(as.numeric(small_$lang_french) ~ k$cluster)
boxplot(as.numeric(small_$lang_spanish) ~ k$cluster)


# to check proportions of is_active by cluster

prop.table(table(small_[k$clust == 1, 5]))
#and data:
small_[k$clust == 1, 5]

prop.table(table(small_[k$clust == 2, 5]))
small_[k$clust == 2, 5]

prop.table(table(small_[k$clust == 3, 5]))
small_[k$clust == 3, 5]

# to check for other vars:
names(small_) #list var names
v <- 9 #set var
prop.table(table(small_[k$clust == 1, v]))
prop.table(table(small_[k$clust == 2, v]))
prop.table(table(small_[k$clust == 3, v]))
# to check specifically for dip_1
dip <- 9 #set var
dipC1 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 1, dip]))))
dipC2 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 2, dip]))))
dipC3 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 3, dip]))))
head(dipC1)
head(dipC2)
head(dipC3)
# to check specifically for has_certif
has_certif <- 21 #set var
prop.table(table(small_[k$clust == 1, has_certif]))
prop.table(table(small_[k$clust == 2, has_certif]))
prop.table(table(small_[k$clust == 3, has_certif]))
# to check specifically for kw_1
kw_1 <- 17 #set var
kw_1C1 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 1, kw_1]))))
kw_1C2 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 2, kw_1]))))
kw_1C3 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 3, kw_1]))))
head(kw_1C1, 10)
head(kw_1C2, 10)
head(kw_1C3, 10)
# to check specifically for sk_1
sk_1 <- 13 #set var
sk_1C1 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 1, sk_1]))))
sk_1C2 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 2, sk_1]))))
sk_1C3 <- orderBy(~-Freq, as.data.frame(prop.table(table(small_[k$clust == 3, sk_1]))))
head(sk_1C1, 10)
head(sk_1C2, 10)
head(sk_1C3, 10)
