
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # avoiding JAVA error
library(MASS)
library(iplots)
library(ggparallel)

parcoord(small_noNA, col=rainbow(length(small_noNA[,1])), var.label=TRUE)
# or selecting only a few vars
parcoord(small_noNA[, c(5,7,8)], col=rainbow(length(small_noNA[,1])), var.label=TRUE)

# interactive plot
ipcp(small_noNA)
ipcp(small_noNA[, c(5,7,8)]) # smaller



########

library(ggplot2)
library(RColorBrewer)

ggparallel(vars=list('is_active', 'sub_division', 'manager_name'),
           data = small_noNA,
  width = 0.1,
  angle = 0,
  color = "white"
)