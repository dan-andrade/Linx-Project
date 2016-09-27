library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(corrplot)
library(ggplot2)
library(reshape2)
library(stringr)
library(doBy)
library(fpc) 
library(topicmodels)

load('C:/Users/Altran/Desktop/BD/29-08/R files/skills.RData')

#skills cleaning

# remove punctuation
skills3 <- skills
skills3$skill_specialty <- gsub("[[:punct:]]", "", as.character(skills3$skill_specialty))
# remove numbers
skills3$skill_specialty <- gsub("[[:digit:]]", "", as.character(skills3$skill_specialty))
# remove term "---"
#skills3$skill_specialty<- gsub("--- ", "", as.character(tolower(skills3$skill_specialty)))
# remove brackets
skills3$skill_specialty <- gsub("\\(|\\)", "", as.character(tolower(skills3$skill_specialty)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
skills3$skill_specialty <- trim(as.character(tolower(skills3$skill_specialty)))
# blanks as 'others'
skills3$skill_specialty <- ifelse(skills3$skill_specialty=='', 'other', skills3$skill_specialty)

# corrections
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('desenvolvimento aplicacional de software', tolower(skills3$skill_specialty)), 'software application development')


skills3$skill_specialty <- tolower(skills3$skill_specialty) #correcting 'OTHERS'
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('architecture  design', skills3$skill_specialty), 'arquitetura e desenho')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('arquitetura e desenho de sistemas', skills3$skill_specialty), 'arquitetura e desenho')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('banking product', skills3$skill_specialty), 'banking')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('banking services', skills3$skill_specialty), 'banking')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('dashboarding  reporting', skills3$skill_specialty), 'dashboarding e relatórios')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('diagnostic maintenance  aftersales', skills3$skill_specialty), 'diagnóstico manutenção e pósvendas')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('financial management', skills3$skill_specialty), 'finance services')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('gestão de processos de negócio e fluxos de trabalho', skills3$skill_specialty), 'gestão de processos ti')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('it test  validation automation', skills3$skill_specialty), 'it technical test  validation')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('packaging  integration', skills3$skill_specialty), 'packaging e integração')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('bancários', skills3$skill_specialty), 'banking')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('finance', skills3$skill_specialty), 'finance services')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('software', skills3$skill_specialty), 'software')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('soluções sap', skills3$skill_specialty), 'sap solutions')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('customer', skills3$skill_specialty), 'customer service')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('cliente', skills3$skill_specialty), 'customer service')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('test', skills3$skill_specialty), 'testing')
skills3$skill_specialty <- replace(skills3$skill_specialty, agrep('transmissão', skills3$skill_specialty), 'transmission')
skills3$skill_specialty <- as.factor(skills3$skill_specialty)

skills3 <- skills3 %>% group_by(employee_id) %>% mutate(sk_nr = row_number())
skills3 <- data.frame(skills3)
skills3$sk_nr <- paste0('sk_', skills3$sk_nr)
#from long to wide
skills.wide <- dcast(skills3, employee_id ~ sk_nr, value.var = 'skill_specialty')
skills.wide[sapply(skills.wide, is.character)] <- lapply(skills.wide[sapply(skills.wide, is.character)],
                                                         as.factor) #convert vars again into factors to use na.tree.replace to recode NAs


# unite skills

sk.wide <- skills.wide
save(sk.wide, file='C:/Users/Altran/Desktop/BD/29-08/R files/sk.wide.RData', ascii=T)
load('C:/Users/Altran/Desktop/BD/29-08/R files/sk.wide.RData')

colns <- capture.output(cat(paste0(names(sk.wide), sep=','))) # to copy/paste in next formula

sk.wide <- unite(sk.wide, col=skills, sk_1, sk_2, sk_3, sk_4, sk_5, sk_6, sk_7, sk_8, sk_9, sk_10, sk_11, sk_12, sk_13, sk_14, sk_15, sk_16, sk_17, sk_18, sk_19, sk_20, sk_21, sk_22, sk_23, sk_24, sk_25, sk_26, sk_27, sep=',', remove=T)
sk.wide$skills <- gsub('NA', '', sk.wide$skills)
sk.wide$skills <- gsub(',,', '', sk.wide$skills)

###

# Text Cleaning

#remove all non graphical characters
usableText=str_replace_all(sk.wide$skills,"[^[:graph:]]", " ")
usableText=str_replace_all(usableText,"[[:punct:]]", " ")
# build a corpus, and specify the source to be character vectors
myCorpus.sks <- Corpus(VectorSource(usableText))
# convert to lower case
myCorpus.sks <- tm_map(myCorpus.sks, content_transformer(tolower))
# remove anything other than space
# removeNumPunct <- function(x) gsub("[^[:space:]]*", "", x)
# myCorpus.sks <- tm_map(myCorpus.sks, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "c")))
myCorpus.sks <- tm_map(myCorpus.sks, removeWords, myStopwords)
# remove extra whitespace
myCorpus.sks <- tm_map(myCorpus.sks, stripWhitespace)
# keep a copy for stem completion later
myCorpus.sksCopy <- myCorpus.sks


# Stemming and stem completion

myCorpus.sks <- tm_map(myCorpus.sks, stemDocument, language='english') # stem words
myCorpus.sks <- tm_map(myCorpus.sks, stemDocument, language='portuguese') # stem words

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x), language='english')
}
myCorpus.sks <- lapply(myCorpus.sks, stemCompletion2, dictionary=myCorpus.sksCopy)
myCorpus.sks <- Corpus(VectorSource(myCorpus.sks))
save(myCorpus.sks, file='C:/users/altran/documents/linx project/myCorpus_sks.RData', ascii=T)
load('C:/users/altran/documents/linx project/myCorpus_sks.RData')


#if problem in stemming
# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus.sks <- replaceWord(myCorpus.sks, "officce", "office")
myCorpus.sks <- replaceWord(myCorpus.sks, "window", "windows")


# count word frequence
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}

#searching for specific words
term1 <- 'sas'
term2 <- 'python'
n.w1.sks <- wordFreq(myCorpus.sks, term1)
n.w2.sks <- wordFreq(myCorpus.sks, term2)
spec.words.sks <- data.frame(Term1=n.w1.sks, Term2=n.w2.sks)
colnames(spec.words.sks) <- c(term1, term2)
rownames(spec.words.sks) <- 'Freq sks'
spec.words.sks


# Create TermDocumentMatrix

tdm.sks <- TermDocumentMatrix(myCorpus.sks,
                             control = list(wordLengths = c(1, Inf)))
tdm.sks


######
######

# n-grams

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # avoiding JAVA error
library(RWeka)
options(mc.cores=1)

tdm.sks_ngram <- TermDocumentMatrix(myCorpus.sks, control=list(tokenize = NGramTokenizer))

######
######


#matrix for those two specific words
idx <- which(dimnames(tdm.sks)$Terms %in% c(term1, term2))
as.matrix(tdm.sks[idx, 1:20])


#Top Frequent Terms

# inspect frequent words
(freq.terms.sks <- findFreqTerms(tdm.sks, lowfreq = 5))

term.freq <- rowSums(as.matrix(tdm.sks))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


# Plot word frequencies

m.sks <- as.matrix(tdm.sks)
v.sks <- sort(rowSums(m.sks),decreasing=TRUE)
d.sks <- data.frame(Word = names(v.sks),Frequency=v.sks)
d.sks$Word <- factor(d.sks$Word, levels = d.sks$Word[order(d.sks$Frequency)])
ggplot(d.sks[1:20,], aes(x=Word, y=Frequency)) + geom_bar(stat="Identity", fill="red") +geom_text(aes(label=Frequency), vjust=-0.20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  ggtitle("Most frequent skills")

sks.words  <- d.sks[1:20,]
rownames(sks.words) <- NULL
write.csv(sks.words, file='c:/users/altran/documents/linx project/sks.words.csv')


# Wordcloud

# calculate the frequency of words and sort it by frequency
word.freq.sks <- sort(rowSums(m.sks), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
wordcloud(words = names(word.freq.sks), freq = word.freq.sks, min.freq = 3,
          random.order = F, colors = pal)


# Correlation between words

dtm.sks <- as.DocumentTermMatrix(tdm.sks)
dcor.sks <- as.character(d.sks[1:10,1])
mc.sks <- cor(as.matrix(dtm.sks[,dcor.sks]), method='pearson', use='pairwise.complete.obs')
corrplot(mc.sks, method='circle', type='full', diag=T, order='hclust', addrect=2, addCoef.col = 'black', main='\n\nCorrelation between skills')

###

# corr for all terms!

dcor.sks <- as.character(d.sks[,1])
mc2.sks <- cor(as.matrix(dtm.sks[,dcor.sks]), method='pearson', use='pairwise.complete.obs')
mc_2.sks <- mc2.sks
mc.cor.tri.sks <- lower.tri(mc_2.sks, diag = TRUE)
# print(mc.cor.tri.sks)
mc_3.sks <- mc_2.sks
mc_3.sks[mc.cor.tri.sks] <- NA
# print(mc_3.sks)
mc_3.sks <- melt(mc_3.sks, na.rm = TRUE)
mc_3.sks <- filter(mc_3.sks, value<1)
mc_3.sks.ord<- orderBy(~-value, mc_3.sks)
f.mc_3.sks <- filter(mc_3.sks.ord, abs(value)>0.8)

f.mc_3.sks[1:10,] #top 10


#which words are associated with a specific term in the docs
term_assoc <- "java"
findAssocs(tdm.sks, term_assoc, corlimit = 0.3)
#or
t1 <- 'sas'
orderBy(~-value, filter(mc_3.sks.ord, (Var1 == t1 | Var2 == t1) & abs(value)>0.2))
t2 <- 'python'
orderBy(~-value, filter(mc_3.sks.ord, (Var1 == t2 | Var2 == t2) & abs(value)>0.2))


###


#correlations as a network
plot(tdm.sks, terms=dcor.sks, corThreshold=0.05)

#Network of Terms

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
library(graph)
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(Rgraphviz)
plot(tdm.sks, term = freq.terms.sks, corThreshold = 0.1, weighting = T)


# Clustering

idf_sk <- weightTfIdf(tdm.sks, normalize = TRUE)# remove sparse terms
idf_sk_2 <- removeSparseTerms(idf_sk, sparse = 0.95)
m2.sks_2 <- as.matrix(idf_sk_2)
# cluster terms
distMatrix.sks_2 <- dist(scale(m2.sks_2))
fit.sks_2 <- hclust(distMatrix.sks_2, method = "ward.D")
plot(fit.sks_2)
rect.hclust(fit.sks_2, k = 5) # cut tree into 3 clusters 

###

m3.sks <- t(m2.sks_2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 3 # number of clusters
kmeansResult <- kmeans(m3.sks, k)
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

# partitioning around medoids with estimation of number of cluster
pamResult.sks <- pamk(m3.sks, metric="manhattan")
k <- pamResult.sks$nc # number of clusters identified
pamResult.sks <- pamResult.sks$pamobject

# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult.sks$medoids)[which(pamResult.sks$medoids[i,]==1)], "\n")
}

# plot clustering result
par(mar=c(5.1,4.1,4.1,2.1))
layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pamResult.sks, col.p = pamResult.sks$clustering)
layout(matrix(1)) # change back to one graph per page


#Topic Modelling

rowTotals.sks <- apply(dtm.sks, 1, sum) #find the sum of words in each Document
dtm.new.sks   <- dtm.sks[rowTotals.sks> 0, ] #remove all docs without words
(empty.docs.sks = as.numeric(names(rowTotals.sks[rowTotals.sks==0]))) # showing the docs without words
#this is useful to keep a 1:1 correspondence between the dtm and the corpus:
empty.rows.sks <- dtm.sks[rowTotals.sks == 0, ]$dimnames[1][[1]]
myCorpus.sks <- myCorpus.sks[-as.numeric(empty.rows.sks)]

lda.sks <- LDA(dtm.new.sks, k = 8) # find 8 topics
term.sks <- terms(lda.sks, 7) # first 7 terms of every topic
(term.sks <- apply(term.sks, MARGIN = 2, paste, collapse = ", "))

###

## add date vars experience or seniority?

topics.sks <- topics(lda.sks, 1) # 1st topic identified for every document (tweet)
new.sk.wide = sk.wide[-c(empty.docs.sks), ] # remove the tweets that correspond to the empty docs
topics.sks <- data.frame(date=as.IDate(sk.wide$career_start_date), topic=topics.sks)
qplot(date, ..count.., data=topics.sks, geom="density",
      fill=term.sks[topic], main='sks') + scale_fill_discrete("Topics")


# analysis of main complaints in the industry
#http://whatphoneplans.co.uk/the-most-complained-about-mobile-phone-operators-in-the-uk/

# 'java' (if possible, look for locations in the text)
java <- sk.wide[grep('java', sk.wide$skills), ]
#sort by variable function
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}


## add var employee_type?

#plot
ggplot(java, aes(reorder_size(employee_type))) + geom_bar(aes(fill=employee_type)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"java\" in data")
#also with 'signal':
oracle <- sk.wide[grep('oracle', sk.wide$skills), ]
ggplot(signal, aes(reorder_size(employee_type))) + geom_bar(aes(fill=employee_type)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"oracle\" in data")

#billing
billing <- sk.wide[grep('bill', sk.wide$text), ]
ggplot(billing, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"bill\" in tweets")

#contract
contract <- sk.wide[grep('contract', sk.wide$text), ]
ggplot(contract, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"contract\" in tweets")
# findAssocs test with term 'contract':
contractAssoc <- findAssocs(tdm.sks, 'contract', corlimit = 0.1)
contractAssoc
#then do the previous analysis with associated terms 'contract' and 'end'
contract.end <- contract[grep('end', contract$text), ]
ggplot(contract.end, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"end\" in tweets containing term \"contract\"")


