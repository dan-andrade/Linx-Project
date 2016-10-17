library(RankAggreg)
library(clValid)
library(dplyr)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.mall.RData')
load('C:/Users/Altran/Desktop/BD/29-08/R files/k2.wide.RData')


# missing values per column

k2.active <- merge(k2.wide, all.small, by='employee_id')
k2.active <- select(k2.active, 1:110, 115)

mv.k2.wide <- sapply(k2.active, function(x){sum(is.na(x))/length(x)})*100
mv.k2.wide
#select columns with missing value less than 15%
k2.wide_new <- subset(k2.active, select = mv.k2.wide < 15 )
k2.wide_new <- na.tree.replace(k2.wide_new)

k2.wide_new <- k2.wide_new[, -1]

k2.wide.smp <- dummy.data.frame(k2.wide_new, names=names(k2.wide_new)[c(1:4)])

setnames(k2.wide.smp, old='is_active', new='not_active')
k2.wide.smp$not_active <- ifelse(k2.wide.smp$not_active=='NO', 1, 0)


# internal validation

result.kws <- clValid(k2.wide.smp, 2:6, clMethods=c("hierarchical","kmeans","pam"),
                      validation=c("internal"))

res.kws <- getRanksWeights(result.kws)

if(require("RankAggreg")) {
  CEWS <- RankAggreg(x=res.kws$ranks, k=10, weights=res$weights, seed=2016, verbose=T,
                     method='CE',
                     distance='Spearman')
  CEWS
}

plot(CEWS)
par(mfrow=c(1,1))


###

# stability

# stability measures

load('C:/Users/Altran/Desktop/BD/29-08/R files/stab.kw.RData')

stab.kws <- clValid(k2.wide.smp, 2:8, clMethods=c("hierarchical","kmeans","pam"),
                    validation="stability")
save(stab.kws, file='C:/Users/Altran/Desktop/BD/29-08/R files/stab.kw.RData', ascii=T)

optimalScores(stab.kws)
plot(stab.kws)

##

hc <- clusters(result.kws,"hierarchical")
plot(hc, cex=0.7, hang=-1, main="Cluster Dendrogram")
two <- cutree(hc, 2)
xtabs(~k2.wide.smp$not_active + two)


