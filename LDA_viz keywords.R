### from https://github.com/brettamdur/skillSimilarities

library(readr)
library(tm)
library(SnowballC)
library(Matrix)
library(lda)
library(LDAvis)
library(servr)

set.seed(2016)

load('C:/Users/Altran/Desktop/BD/29-08/R files/kw.wide.RData')
txt <- kw.wide$keyword

# pre-processing
txt <- gsub("'", "", txt)  # remove apostrophes
txt <- gsub("[[:punct:]]", " ", txt)  # replace punctuation with space
txt <- gsub("[[:cntrl:]]", " ", txt)  # replace control characters with space
txt <- gsub("^[[:space:]]+", "", txt) # remove whitespace at beginning of documents
txt <- gsub("[[:space:]]+$", "", txt) # remove whitespace at end of documents
txt <- tolower(txt)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(txt, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)
W <- length(vocab)  
doc.length <- sapply(documents, function(x) sum(x[2, ])) 
N <- sum(doc.length)  
term.frequency <- as.integer(term.table)
alpha <- 0.02
eta <- 0.02
K <- 10
fit.lda <- lda.collapsed.gibbs.sampler(document=documents,
                            vocab=vocab,
                            K=K,
                            num.iterations=100,
                            alpha=alpha,
                            eta = eta,
                            initial = NULL,
                            burnin = 0,
                            compute.log.likelihood = TRUE)

### Visualizing the fitted model with LDAvis
theta <- t(apply(fit.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit.lda$topics) + eta, 2, function(x) x/sum(x)))

results.lda <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = results.lda$phi, 
                   theta = results.lda$theta, 
                   doc.length = results.lda$doc.length, 
                   vocab = results.lda$vocab, 
                   term.frequency = results.lda$term.frequency)

serVis(json, out.dir = './', open.browser = T)
system("mv index.html results.html")

