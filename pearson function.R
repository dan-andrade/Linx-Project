library(dplyr)
library(doBY)
library(vcs)

pearson <- function(x) {
  ps <- xtabs(~ x+smp_all_noNA$not_active, smp_all_noNA)
  print(assocstats(ps)) #Pearson
}

pearson.small <- sapply(smp_all_noNA[, -46], function(x) pearson(x))
pearson.small.df <- as.data.frame(pearson.small)

pearson.un <- as.data.frame(unlist(pearson.small.df))
pearson.un$var <- rownames(pearson.un)
pearson.df <- pearson.un[grep('chisq_tests6', pearson.un$var), ]
setnames(pearson.df, old='unlist(pearson.small.df)', new='pearson')
pearson.df <- orderBy(~-pearson, pearson.df)
rownames(pearson.df) <- seq(1, nrow(pearson.df), 1)
pearson.df$var <- gsub('.chisq_tests6', '', pearson.df$var)
pearson.df$pearson <- round(pearson.df$pearson, 6)
head(pearson.df, 20)
