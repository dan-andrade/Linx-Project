library(arules)

load('C:/Users/Altran/Desktop/BD/29-08/R files/k2.RData')

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

all.small_2 <- all.small
all.small_2 <- as.data.frame(apply(all.small_2, 2, function(x) as.factor(x)))
rules.kw <- apriori(all.small_2)

quality(rules.kw) <- round(quality(rules.kw), digits=3)
arules::inspect(rules.kw)


# rules with rhs containing "is_active" only
rules <- apriori(all.small_2, control = list(verbose=F), parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("is_active=NO", "is_active=YES"),default="lhs"))

quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
