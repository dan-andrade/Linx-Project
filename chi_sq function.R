library(dplyr)
library(doBY)
library(vcd)

# small_noNA ds

chi.sq <- function(x) {
  mt <- xtabs(~ x+small_noNA$is_active, small_noNA)
  print(summary(mt)) #Chi-square
}

chi.sq(small_noNA$position) # testing

chi.small <- sapply(small_noNA[, -5], function(x) chi.sq(x))
chi.small.df <- t(as.data.frame(chi.small)[6, ])
chi.small.df <- as.data.frame(chi.small.df)
chi.small.df <- as.data.frame(chi.small.df$p.value)
chi.small.df <- as.data.frame(t(chi.small.df))
colnames(chi.small.df) <- 'p.value'
chi.small.df$p.value <- round(chi.small.df$p.value, 6)
orderBy(~p.value, chi.small.df)


# strength of association
# cramer's v (nominal vs nomimal)

cramer <- function(x) {
  cm <- xtabs(~ x+small_noNA$is_active, small_noNA)
  print(assocstats(cm)) #Cramer's V
}

cramer.small <- sapply(small_noNA[, -5], function(x) cramer(x))
cramer.small.df <- as.data.frame(cramer.small)

cramer.un <- as.data.frame(unlist(cramer.small.df))
cramer.un$var <- rownames(cramer.un)
cramer.df <- cramer.un[grep('cramer', cramer.un$var), ]
setnames(cramer.df, old='unlist(cramer.small.df)', new='cramer')
cramer.df <- orderBy(~-cramer, cramer.df)
rownames(cramer.df) <- seq(1, nrow(cramer.df), 1)
cramer.df$var <- gsub('.cramer', '', cramer.df$var)
cramer.df


# smp.new ds

chi.sq.smp <- function(x) {
  mt <- xtabs(~ x+smp.new$is_active, smp.new)
  print(summary(mt)) #Chi-square
}

chi.sq.smp(smp.new$manager_nameMarco_FERREIRA) # testing
chi.sq.smp(smp.new$manager_nameCélia_REIS) # testing


# all ds
chi.smp <- sapply(smp.new[, -40], function(x) chi.sq.smp(x))
chi.smp.df <- t(as.data.frame(chi.smp)[6, ])
chi.smp.df <- as.data.frame(chi.smp.df)
chi.smp.df <- as.data.frame(chi.smp.df$p.value)
chi.smp.df <- as.data.frame(t(chi.smp.df))
colnames(chi.smp.df) <- 'p.value'
chi.smp.df$p.value <- round(chi.smp.df$p.value, 6)
head(orderBy(~p.value, chi.smp.df), 20)