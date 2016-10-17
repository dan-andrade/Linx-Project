# https://arxiv.org/pdf/1112.0295.pdf

library(Amelia)
library(rattle)
library(ClustOfVar)
library(randomForest)

load('C:/Users/Altran/Desktop/BD/29-08/R files/smp_all_noNA.RData')

smp_all_noNA$at_lisbon <- as.numeric(smp_all_noNA$at_lisbon)
smp_all_noNA$at_lisbon <- ifelse(smp_all_noNA$at_lisbon==2, 1, 0)
vars <- names(smp_all_noNA)
smp_all_noNA[sapply(smp_all_noNA, is.factor)] <- lapply(smp_all_noNA[sapply(smp_all_noNA, is.factor)],
                                                       as.numeric) 
smp_all_noNA$is_active <- ifelse(smp_all_noNA$is_active==2, 1, 0)

dsname <- 'smp_all_noNA'
dst <- get(dsname)
names(dst) <- normVarNames(names(dst))
vars <- names(dst)
target <- dst$is_active
risk <- "risk_mm"
id <- c("employee_id")
# Ignore the IDs and the risk variable.
ignore <- union(id, if (exists("risk")) risk)
# Ignore variables which are completely missing.
mvc <- sapply(dst[vars], function(x) sum(is.na(x))) # Missing value count
mvn <- names(dst)[(which(mvc == nrow(dst)))] # Missing var names
ignore <- union(ignore, mvn)
# Initialise the variables
vars <- setdiff(vars, ignore)
# Variable roles
inputc <- setdiff(vars, target)
inputi <- sapply(inputc, function(x) which(x == names(dst)), USE.NAMES=FALSE)
numi <- intersect(inputi, which(sapply(dst, is.numeric)))
numc <- names(dst)[numi]
cati <- intersect(inputi, which(sapply(dst, is.factor)))
catc <- names(dst)[cati]


# Hierarchical tree (w/ cutting)

ds_qt <- dst[numi] #qt dataset
ds_ql <- dst[cati] #qualitative dataset
tree <- hclustvar(ds_qt, X.quali=NULL)
plot(tree)

# sub-ploting
dendro <- cut(as.dendrogram(tree), h=20)
dendro

#branch 1
plot(dendro$lower[[1]], main = "Branch 1") # still too big
dendro1=cut(dendro$lower[[1]],h=30)
dendro1
plot(dendro1$lower[[1]],main = "Branch 1")
plot(dendro1$lower[[2]],main = "Branch 2")
dendro1.2 <- cut(dendro1$lower[[1]],h=30)
dendro1.2
plot(dendro1.2$lower[[1]],main = "Branch 1 (1.2)")
plot(dendro1.2$lower[[2]],main = "Branch 2 (1.2)")

# ...

#branch 2
plot(dendro$lower[[2]], main = "Branch 2") # still too big
dendro2=cut(dendro$lower[[2]],h=30)
dendro2
plot(dendro2$lower[[1]])


#choice of the number of clusters
stability(tree,B=40)
part <- cutreevar(tree, 15)
print(part)
summary(part)
part$var$"cluster1" #shwoing cluster 1

part_hier <- cutreevar(tree,6)
part_hier$var$"cluster1"


##
# ClustOfVar::kmeansvar for 'mixed' vars

part_km <- kmeansvar(ds_qt, ds_ql, init=2,
                     iter.max = 150, nstart = 10, matsim = FALSE)
print(summary(part_km))
#gain in cohesion of the partition
part_km$E
part_hier$E #with the k-means type, is smaller than with the hierarchical clustering algorithm



#################

## to check from numeric vars with k-means

library(fpc)

nk <- 1:20
model <- kmeansruns(scale(dst[numi]), krange=nk, criterion="asw")
model$bestk

nclust <- 4
model <- m.kms <- kmeans(scale(dst[numi]), nclust)
dscm <- melt(model$centers)
names(dscm) <- c("Cluster", "Variable", "Value")
dscm$Cluster <- factor(dscm$Cluster)
dscm$Order <- as.vector(sapply(1:length(numi), rep, nclust))
p <- ggplot(dscm,
            aes(x=reorder(Variable, Order),
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + ylim(-1, 1)
p <- p + ggtitle("Clusters profile (variables were scaled)")
p



###############


