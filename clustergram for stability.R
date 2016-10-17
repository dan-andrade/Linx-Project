source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")

clustergram(as.matrix(smp_all_noNA[ , -46]), k.range = 2:5, line.width = 0.004)


# testing stability
#by running the algorithm 6 times and compare
set.seed(2016)
Data <- as.matrix(smp_all_noNA[ , -46]) # notice I am scaling the vectors)
par(cex.lab = 1.2, cex.main = .7)
par(mfrow = c(3,2))
for(i in 1:6) clustergram(Data, k.range = 2:4 , line.width = .004, add.center.points = T)
