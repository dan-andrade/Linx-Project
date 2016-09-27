# https://arxiv.org/pdf/1112.0295.pdf

library(Amelia)
library(rattle)
library(ClustOfVar)
library(randomForest)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)

small_[sapply(small_, function(x) nlevels(x)==2)] <- lapply(small_[sapply(small_, function(x) nlevels(x)==2)],
                                                            as.numeric) 

small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

small_cl <- small_
small_cl[sapply(small_cl, is.character)] <- lapply(small_cl[sapply(small_cl, is.character)],
                                                         as.factor) 
vars <- names(small_cl)
if (sum(is.na(small_cl[vars]))) small_cl[vars] <- na.roughfix(small_cl[vars]) #impute by median/mode (randomForest)




dsname <- 'small_cl'
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
tree <- hclustvar(ds_qt, ds_ql)
plot(tree)
#choice of the number of clusters
stability(tree,B=40)
part <- cutreevar(tree, 3)
print(part)
summary(part)

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


