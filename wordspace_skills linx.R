# from https://cran.rstudio.com/web/packages/wordspace/vignettes/wordspace-intro.html

library(tm)
library(wordspace)
library(dplyr)


load('C:/users/altran/documents/linx project/myCorpus_sks.RData')


#if problem in stemming
# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus.sks <- replaceWord(myCorpus.sks, "produto", "product")
myCorpus.sks <- replaceWord(myCorpus.sks, "previsão", "prediction")
myCorpus.sks <- replaceWord(myCorpus.sks, "plataforms", "plataform")
myCorpus.sks <- replaceWord(myCorpus.sks, "manutenção", "maintenance")
myCorpus.sks <- replaceWord(myCorpus.sks, "inteligente", "intelligence")
myCorpus.sks <- replaceWord(myCorpus.sks, "dados", "data")
myCorpus.sks <- replaceWord(myCorpus.sks, "analyse", "analysis")
myCorpus.sks <- replaceWord(myCorpus.sks, "distribuées", "distributed")
myCorpus.sks <- replaceWord(myCorpus.sks, "recursos", "resources")
myCorpus.sks <- replaceWord(myCorpus.sks, "distribuées", "distributed")
myCorpus.sks <- replaceWord(myCorpus.sks, "requisitos", "requirement")
myCorpus.sks <- replaceWord(myCorpus.sks, "segurança", "securities")
myCorpus.sks <- replaceWord(myCorpus.sks, "visualização", "visualisation")


# Create TermDocumentMatrix

tdm.sks <- TermDocumentMatrix(myCorpus.sks,
                              control = list(wordLengths = c(1, Inf)))
tdm.sks

tdm.sks.mx <- as.matrix(tdm.sks)


# transform into a term-term adjacency matrix
termWordspaceMatrix.sk <- tdm.sks.mx %*% t(tdm.sks.mx)

pair.distances("data", "web", termWordspaceMatrix.sk, method="cosine", convert=FALSE)

nearest.neighbours(termWordspaceMatrix.sk, "data")

nn.mat <- nearest.neighbours(termWordspaceMatrix.sk,
                             "data",
                             dist.matrix=TRUE,
                             n=9) #n=9 because zero distance between objects 10 and 11
plot(nn.mat)

data <- termWordspaceMatrix.sk["data", ] # extract row vectors from matrix
science <- termWordspaceMatrix.sk["web", ]
nearest.neighbours(termWordspaceMatrix.sk, data + science)


