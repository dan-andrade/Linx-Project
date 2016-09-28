library(arules)
library(arulesViz)

# http://www.rdatamining.com/examples/association-rules


load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

all.small_2 <- all.small
all.small_2 <- as.data.frame(apply(all.small_2, 2, function(x) as.factor(x)))
rules.kw <- apriori(all.small_2)

quality(rules.kw) <- round(quality(rules.kw), digits=3)
# arules::inspect(rules.kw)


# rules with rhs containing "is_active" only
rules <- apriori(all.small_2,
                 control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("is_active=NO", "is_active=YES"), default="lhs"))

### testing:
notActive <- apriori(all.small_2, control = list(verbose=F),
                     parameter = list(minlen=2, supp=0.005, conf=0.5),
                     appearance = list(default="none", rhs=c("is_active=NO"),
                          lhs=c("kw_5=oracle")))
###


quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")

inspect(head(rules.sorted, 20))

# remove redundant rules
subset.matrix <- is.subset(rules.sorted[1:100], rules.sorted[1:100]) # only 100 due to memory issues
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]

inspect(head(rules.pruned, 20))


# visualization

plot(head(rules.pruned, 20), method = "grouped")
plot(head(rules.pruned, 20), method = "graph")
plot(head(rules.pruned, 20), method = "paracoord", control = list(reorder = TRUE))
