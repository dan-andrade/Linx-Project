# from https://cran.rstudio.com/web/packages/wordspace/vignettes/wordspace-intro.html

library(tm)
library(wordspace)
library(dplyr)


load('C:/users/altran/documents/linx project/myCorpus_kws.RData')


#if problem in stemming
# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus.kws <- replaceWord(myCorpus.kws, "officce", "office")
myCorpus.kws <- replaceWord(myCorpus.kws, "window", "windows")

# Create TermDocumentMatrix

tdm.kws <- TermDocumentMatrix(myCorpus.kws,
                              control = list(wordLengths = c(1, Inf)))
tdm.kws

tdm.kws.mx <- as.matrix(tdm.kws)


# transform into a term-term adjacency matrix
termWordspaceMatrix <- tdm.kws.mx %*% t(tdm.kws.mx)

pair.distances("java", "xml", termWordspaceMatrix, method="cosine", convert=FALSE)

nearest.neighbours(termWordspaceMatrix, "java")

nn.mat <- nearest.neighbours(termWordspaceMatrix, "java", dist.matrix=TRUE)
plot(nn.mat)

data <- termWordspaceMatrix["data", ] # extract row vectors from matrix
science <- termWordspaceMatrix["sciences", ]
nearest.neighbours(termWordspaceMatrix, data + science)

