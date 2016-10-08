library(RankAggreg)
library(clValid)
library(dplyr)

load('C:/Users/Altran/Desktop/BD/29-08/R files/smp.new.RData')


# internal validation

result <- clValid(smp.new, 2:20, clMethods=c("hierarchical","kmeans","pam"),
                  validation=c("internal"))

res <- getRanksWeights(result)

if(require("RankAggreg")) {
  CEWS <- RankAggreg(x=res$ranks, k=10, weights=res$weights, seed=2006, verbose=T,
                     method='CE',
                     distance='Spearman')
  CEWS
}

plot(CEWS)


# stability

# stability measures

load('C:/Users/Altran/Desktop/BD/29-08/R files/stab.RData')

stab <- clValid(smp.new, 2:8, clMethods=c("hierarchical","kmeans","pam"),
                validation="stability")
save(stab, file='C:/Users/Altran/Desktop/BD/29-08/R files/stab.RData', ascii=T)

optimalScores(stab)
plot(stab)

##

hc <- clusters(result,"hierarchical")
plot(hc, cex=0.7, hang=-1, main="Mouse Cluster Dendrogram")
two <- cutree(hc, 2)
xtabs(~smp.new$is_active + two)


