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

load('C:/Users/Altran/Desktop/BD/29-08/R files/keywords.RData')

### keyword cleaning

# sql query
keywords2 <- keywords

keywords2 <- sqldf("select *,
                   case 
                   when upper(keyword) like '%.NET%' then '.NET'
                   when upper(keyword) like '%EXCEL%' then 'Excel'
                   when upper(keyword) like '%ALCATELLUCENT%' then 'alcatellucent'
                   when upper(keyword) like '%v?rgula(%' then 'other'
                   when upper(keyword) like '%semi-colon (%' then 'other'
                   when upper(keyword) like '%WORD%' then 'Word'
                   when upper(keyword) like '%BPM%' then 'BPM'
                   when upper(keyword) like '%JBOSS%' then 'JBoss'
                   when upper(keyword) like '%ABAP%' then 'ABAP'
                   when upper(keyword) like '%FIREWORKS%' then 'Fireworks'
                   when upper(keyword) like '%ILLUSTRATOR%' then 'Illustrator'
                   when upper(keyword) like '%INDESIGN%' then 'InDesign'
                   when upper(keyword) like '%LIFECYCLE%' then 'LifeCycle'
                   when upper(keyword) like '%PHOTOSHOP%' then 'Photoshop'
                   when upper(keyword) like '%PREMIERE%' then 'Premiere'
                   when upper(keyword) like '%SCOUT%' then 'Scout'
                   when upper(keyword) like '%OFFICE%' then 'Office'
                   when upper(keyword) like '%AGILE%' then 'Agile'
                   when upper(keyword) like '%AJAX%' then 'Ajax'
                   when upper(keyword) like '%ALTAMIRA%' then 'Altamira'
                   when upper(keyword) like '%ANALISTA%' then 'Analista'
                   when upper(keyword) like '%AN?LISE%' then 'Analista'
                   when upper(keyword) like '%ANALYSIS%' then 'Analista'
                   when upper(keyword) like '%ANGULAR%' then 'Angular'
                   when upper(keyword) like '%APACHE%' then 'Apache'
                   when upper(keyword) like '%ARIBA%' then 'ARIBA'
                   when upper(keyword) like '%AS400%' then 'AS400'
                   when upper(keyword) like '%AUDITORIA%' then 'auditoria'
                   when upper(keyword) like '%ASP%' then 'ASP'
                   when upper(keyword) like '%BMC%' then 'BMC'
                   when upper(keyword) like '%BOOTSARP%' then 'Bootstrap'
                   when upper(keyword) like '%BUGTRACK%' then 'Bugtracking'
                   when upper(keyword) like 'C LANG%' then 'C'
                   when upper(keyword) like 'C PROG%' then 'C'
                   when upper(keyword) like '%C#%' then 'C#'
                   when upper(keyword) like '%C++%' then 'C++'
                   when upper(keyword) like '%CICS%' then 'CICS'
                   when upper(keyword) like '%CITRIX%' then 'Citrix'
                   when upper(keyword) like '%CLIPPER%' then 'Clipper'
                   when upper(keyword) like '%COBOL%' then 'Cobol'
                   when upper(keyword) like '%CONSULT%' then 'Consultoria'
                   when upper(keyword) like '%CONTROLL%' then 'Controlling'
                   when upper(keyword) like '%CRYSTAL%' then 'Crystal'
                   when upper(keyword) like '%CSS%' then 'CSS'
                   when upper(keyword) like '%DELPHI%' then 'Delphi'
                   when upper(keyword) like '%DISASTER%' then 'Disaster Recovery'
                   when upper(keyword) like '%EDUCA??O%' then 'Educa??o'
                   when upper(keyword) like '%LINUX%' then 'Linux'
                   when upper(keyword) like '%ENTIDY%' then 'Entity Framework'
                   when upper(keyword) like '%ETL%' then 'ETL'
                   when upper(keyword) like '%JS%' then 'JS'
                   when upper(keyword) like '%JAVASCRIPT%' then 'JS'
                   when upper(keyword) like '%JAVA%' then 'JAVA'
                   when upper(keyword) like '%GEST?O%' then 'Gest?o'
                   when upper(keyword) like '%GITHUB%' then 'GITHUB'
                   when upper(keyword) like '%GIT%' then 'GIT'
                   when upper(keyword) like '%GOOGLE%' then 'Google'
                   when upper(keyword) like '%HADOOP%' then 'Hadoop'
                   when upper(keyword) like '%HP%' then 'HP'
                   when upper(keyword) like '%HTML%' then 'HTML'
                   when upper(keyword) like '%HTTP%' then 'HTTP'
                   when upper(keyword) like '%IBM%' then 'IBM'
                   when upper(keyword) like '%INFORMIX%' then 'Informix'
                   when upper(keyword) like '%ITIL%' then 'ITIL'
                   when upper(keyword) like '%JOOMLA%' then 'JOOMLA'
                   when upper(keyword) like '%JQUERY%' then 'JQUERY'
                   when upper(keyword) like '%LEAN%' then 'LEAN'
                   when upper(keyword) like '%LINQ%' then 'LINQ'
                   when upper(keyword) like '%LINUX%' then 'LINUX'
                   when upper(keyword) like '%MAINFRAME%' then 'Mainframe'
                   when upper(keyword) like '%MATEM?TICA%' then 'Matem?tica'
                   when upper(keyword) like '%MATH%' then 'Matem?tica'
                   when upper(keyword) like '%BIZTALK%' then 'Microsoft BizTalk'
                   when upper(keyword) like '%OFFICE%' then 'Office'
                   when upper(keyword) like '%SHAREPOINT%' then 'SharePoint'
                   when upper(keyword) like '%VISUAL STUD%' then 'Visual Studio'
                   when upper(keyword) like '%WINDOWS%' then 'Windows'
                   when upper(keyword) like '%ACCESS%' then 'Access'
                   when upper(keyword) like '%MVC%' then 'MVC'
                   when upper(keyword) like '%NETBACKUP%' then 'Netbackup'
                   when upper(keyword) like '%NOVELL%' then 'NOVELL'
                   when upper(keyword) like '%ORACLE%' then 'Oracle'
                   when upper(keyword) like '%OUTSYSTEMS%' then 'Outsystems'
                   when upper(keyword) like '%PACBASE%' then 'Packbase'
                   when upper(keyword) like '%PAGAMENTOS%' then 'Payments'
                   when upper(keyword) like '%PLANEAMENTO%' then 'Planeamento'
                   when upper(keyword) like '%PLANO %' then 'NET'
                   when upper(keyword) like '%POWERCENTER%' then 'Power Center'
                   when upper(keyword) like '%QUALIDADE%' then 'Qualidade'
                   when upper(keyword) like '%QUALITY%' then 'Qualidade'
                   when upper(keyword) like '%REPORT%' then 'Reporting'
                   when upper(keyword) like '%BANC%' then 'BANCA'
                   when upper(keyword) like '%BANK%' then 'BANCA'
                   when upper(keyword) like '%SAP%' then 'SAP'
                   when upper(keyword) like '%SAS%' then 'SAS'
                   when upper(keyword) like '%Serena%' then 'Serena'
                   when upper(keyword) like '%SHELLSCRIPT%' then 'Shell Script'
                   when upper(keyword) like '%SOAP%' then 'SOAP'
                   when upper(keyword) like '%SUMTOTAL%' then 'SumTotal'
                   when upper(keyword) like '%SUPPORT%' then 'Support'
                   when upper(keyword) like '%SVN%' then 'SVN'
                   when upper(keyword) like '%SYBASE%' then 'Sybase'
                   when upper(keyword) like '%SQL%' then 'SQL'
                   when upper(keyword) like '%TEAM FOUNDATION%' then 'Team Foundation'
                   when upper(keyword) like '%TELERIK%' then 'Telerik'
                   when upper(keyword) like '%TEST%' then 'Testes'
                   when upper(keyword) like '%TFS%' then 'TFS'
                   when upper(keyword) like '%TOOLBOOK%' then 'Toolbook'
                   when upper(keyword) like '%UML%' then 'UML'
                   when upper(keyword) like '%UNISYS%' then 'Unisys'
                   when upper(keyword) like '%VB%' then 'VB'
                   when upper(keyword) like '%WCF%' then 'WCF'
                   when upper(keyword) like '%WEB%' then 'WEB'
                   when upper(keyword) like '%WINDOWS%' then 'Windows'
                   when upper(keyword) like '%XML%' then 'XML'
                   when upper(keyword) like '%XSL%' then 'XSL'
                   else keyword
                   end as k1_new
                   from keywords2")

# remove punctuation
keywords2$k1_new <- gsub("[[:punct:]]", "", as.character(keywords2$k1_new))
# remove numbers
keywords2$k1_new <- gsub("[[:digit:]]", "", as.character(keywords2$k1_new))
# remove "Microsoft"
keywords2$k1_new <- gsub("microsoft ", "", as.character(tolower(keywords2$k1_new)))
# remove brackets
keywords2$k1_new <- gsub("\\(|\\)", "", as.character(tolower(keywords2$k1_new)))
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
keywords2$k1_new <- trim(as.character(tolower(keywords2$k1_new)))
# blanks as 'others'
keywords2$k1_new <- ifelse(keywords2$k1_new=='', 'other', keywords2$k1_new)

# corrections
keywords2$k1_new <- replace(keywords2$k1_new, agrep('alcatellucent', tolower(keywords2$k1_new)), 'alcatellucent')

# combine levels of keywords
# keywords2$k1_new <- combine.levels(keywords2$k1_new, minlev = 0.0005)

#corrections
keywords2$k1_new <- replace(keywords2$k1_new, agrep('OTHER', keywords2$k1_new), 'other')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('android studio', keywords2$k1_new), 'android')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('github', keywords2$k1_new), 'git')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('ms visio', keywords2$k1_new), 'visio')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('project management', keywords2$k1_new), 'project')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('rest architecture', keywords2$k1_new), 'rest')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('shell scripting', keywords2$k1_new), 'shell script')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('shell scripting ksh sh bash…', keywords2$k1_new), 'shell script')
keywords2$k1_new <- replace(keywords2$k1_new, agrep('tems discovery', keywords2$k1_new), 'tems')
#keywords2$k1_new <- replace(keywords2$k1_new, agrep('vb', keywords2$k1_new), 'visual basic')




k2 <- keywords2


### transpositions from long to wide format

# keywords

#create index for each keyword by ID
k2 <- keywords2 %>% group_by(employee_id) %>% mutate(kw_nr = row_number())
k2 <- data.frame(k2)
k2$kw_nr <- paste0('kw_', k2$kw_nr)
#from long to wide
k2.wide <- dcast(k2, employee_id ~ kw_nr, value.var = 'k1_new')
k2.wide[sapply(k2.wide, is.character)] <- lapply(k2.wide[sapply(k2.wide, is.character)],
                                                 as.factor) #convert vars again into factors to use na.tree.replace to recode NAs

# perc distribution of NAs

# k2.wide[k2.wide=='NA'] <- NA # if NAs are 'NA'
NA_perc.kw <- sapply(k2.wide, function(y) round(sum(length(which(is.na(y))))*100/length(y), 1))
NA_perc.kw <- data.frame(NA_perc.kw)
orderBy(~-NA_perc.kw, NA_perc.kw)

# to delete vars with more than a certain $ of NAs
# k2.wide_small <- k2.wide[, colSums(is.na(k2.wide)) < nrow(k2.wide) * 0.7]

# ?
# k2.wide_small <- na.tree.replace(k2.wide_small)

# or when 'NA'
NAfact_perc.kw <- sapply(k2.wide, function(y) round(as.numeric(length(y[y=='NA'])*100/length(y)), 1))
k2.wide_small <- na.tree.replace(k2.wide_small)


# unite keywords

kw.wide <- k2.wide

colns <- capture.output(cat(paste0(names(kw.wide), sep=','))) # to copy/paste in next formula

kw.wide <- unite(kw.wide, col=keywords, kw_1, kw_10, kw_100, kw_101, kw_102, kw_103, kw_104, kw_105, kw_106, kw_107, kw_108, kw_109, kw_11, kw_12, kw_13, kw_14, kw_15, kw_16, kw_17, kw_18, kw_19, kw_2, kw_20, kw_21, kw_22, kw_23, kw_24, kw_25, kw_26, kw_27, kw_28, kw_29, kw_3, kw_30, kw_31, kw_32, kw_33, kw_34, kw_35, kw_36, kw_37, kw_38, kw_39, kw_4, kw_40, kw_41, kw_42, kw_43, kw_44, kw_45, kw_46, kw_47, kw_48, kw_49, kw_5, kw_50, kw_51, kw_52, kw_53, kw_54, kw_55, kw_56, kw_57, kw_58, kw_59, kw_6, kw_60, kw_61, kw_62, kw_63, kw_64, kw_65, kw_66, kw_67, kw_68, kw_69, kw_7, kw_70, kw_71, kw_72, kw_73, kw_74, kw_75, kw_76, kw_77, kw_78, kw_79, kw_8, kw_80, kw_81, kw_82, kw_83, kw_84, kw_85, kw_86, kw_87, kw_88, kw_89, kw_9, kw_90, kw_91, kw_92, kw_93, kw_94, kw_95, kw_96, kw_97, kw_98, kw_99, sep=',', remove=T)
kw.wide$keywords <- gsub('NA', '', kw.wide$keywords)
kw.wide$keywords <- gsub(',,', '', kw.wide$keywords)

###

# Text Cleaning

#remove all non graphical characters
usableText=str_replace_all(kw.wide$keyword,"[^[:graph:]]", " ")
usableText=str_replace_all(usableText,"[[:punct:]]", " ")
# build a corpus, and specify the source to be character vectors
myCorpus.kws <- Corpus(VectorSource(usableText))
# convert to lower case
myCorpus.kws <- tm_map(myCorpus.kws, content_transformer(tolower))
# remove anything other than space
# removeNumPunct <- function(x) gsub("[^[:space:]]*", "", x)
# myCorpus.kws <- tm_map(myCorpus.kws, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "c")))
myCorpus.kws <- tm_map(myCorpus.kws, removeWords, myStopwords)
# remove extra whitespace
myCorpus.kws <- tm_map(myCorpus.kws, stripWhitespace)
# keep a copy for stem completion later
myCorpus.kwsCopy <- myCorpus.kws


# Stemming and stem completion

myCorpus.kws <- tm_map(myCorpus.kws, stemDocument, language='english') # stem words
myCorpus.kws <- tm_map(myCorpus.kws, stemDocument, language='portuguese') # stem words

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x), language='english')
}
myCorpus.kws <- lapply(myCorpus.kws, stemCompletion2, dictionary=myCorpus.kwsCopy)
myCorpus.kws <- Corpus(VectorSource(myCorpus.kws))
save(myCorpus.kws, file='C:/users/altran/documents/linx project/myCorpus_kws.RData', ascii=T)
load('C:/users/altran/documents/linx project/myCorpus_kws.RData')


#if problem in stemming
# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus.kws <- replaceWord(myCorpus.kws, "officce", "office")
myCorpus.kws <- replaceWord(myCorpus.kws, "window", "windows")


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
n.w1.kws <- wordFreq(myCorpus.kws, term1)
n.w2.kws <- wordFreq(myCorpus.kws, term2)
spec.words.kws <- data.frame(Term1=n.w1.kws, Term2=n.w2.kws)
colnames(spec.words.kws) <- c(term1, term2)
rownames(spec.words.kws) <- 'Freq kws'
spec.words.kws


# Create TermDocumentMatrix

tdm.kws <- TermDocumentMatrix(myCorpus.kws,
                              control = list(wordLengths = c(1, Inf)))
tdm.kws


######
######

# n-grams

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # avoiding JAVA error
library(RWeka)
options(mc.cores=1)

tdm.kws_ngram <- TermDocumentMatrix(myCorpus.kws, control=list(tokenize = NGramTokenizer))

######
######


#matrix for those two specific words
idx <- which(dimnames(tdm.kws)$Terms %in% c(term1, term2))
as.matrix(tdm.kws[idx, 1:20])


#Top Frequent Terms

# inspect frequent words
(freq.terms.kws <- findFreqTerms(tdm.kws, lowfreq = 5))

term.freq <- rowSums(as.matrix(tdm.kws))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


# Plot word frequencies

m.kws <- as.matrix(tdm.kws)
v.kws <- sort(rowSums(m.kws),decreasing=TRUE)
d.kws <- data.frame(Word = names(v.kws),Frequency=v.kws)
d.kws$Word <- factor(d.kws$Word, levels = d.kws$Word[order(d.kws$Frequency)])
ggplot(d.kws[1:20,], aes(x=Word, y=Frequency)) + geom_bar(stat="Identity", fill="red") +geom_text(aes(label=Frequency), vjust=-0.20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  ggtitle("Most frequent keywords")

kws.words  <- d.kws[1:20,]
rownames(kws.words) <- NULL
write.csv(kws.words, file='c:/users/altran/documents/linx project/kws.words.csv')


# Wordcloud

# calculate the frequency of words and sort it by frequency
word.freq.kws <- sort(rowSums(m.kws), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
# plot word cloud
wordcloud(words = names(word.freq.kws), freq = word.freq.kws, min.freq = 3,
          random.order = F, colors = pal)


# Correlation between words

dtm.kws <- as.DocumentTermMatrix(tdm.kws)
dcor.kws <- as.character(d.kws[1:10,1])
mc.kws <- cor(as.matrix(dtm.kws[,dcor.kws]), method='pearson', use='pairwise.complete.obs')
corrplot(mc.kws, method='circle', type='full', diag=T, order='hclust', addrect=2, addCoef.col = 'black', main='\n\nCorrelation between keywords')

###

# corr for all terms!

dcor.kws <- as.character(d.kws[,1])
mc2.kws <- cor(as.matrix(dtm.kws[,dcor.kws]), method='pearson', use='pairwise.complete.obs')
mc_2.kws <- mc2.kws
mc.cor.tri.kws <- lower.tri(mc_2.kws, diag = TRUE)
# print(mc.cor.tri.kws)
mc_3.kws <- mc_2.kws
mc_3.kws[mc.cor.tri.kws] <- NA
# print(mc_3.kws)
mc_3.kws <- melt(mc_3.kws, na.rm = TRUE)
mc_3.kws <- filter(mc_3.kws, value<1)
mc_3.kws.ord<- orderBy(~-value, mc_3.kws)
f.mc_3.kws <- filter(mc_3.kws.ord, abs(value)>0.8)

f.mc_3.kws[1:10,] #top 10


#which words are associated with a specific term in the docs
term_assoc <- "java"
findAssocs(tdm.kws, term_assoc, corlimit = 0.2)
#or
t1 <- 'sas'
orderBy(~-value, filter(mc_3.kws.ord, (Var1 == t1 | Var2 == t1) & abs(value)>0.2))
t2 <- 'python'
orderBy(~-value, filter(mc_3.kws.ord, (Var1 == t2 | Var2 == t2) & abs(value)>0.2))


###


#correlations as a network
plot(tdm.kws, terms=dcor.kws, corThreshold=0.05)

#Network of Terms

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
library(graph)
# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(Rgraphviz)
plot(tdm.kws, term = freq.terms.kws, corThreshold = 0.1, weighting = T)


# Clustering

idf <- weightTfIdf(tdm.kws, normalize = TRUE)# remove sparse terms
idf_2 <- removeSparseTerms(idf, sparse = 0.95)
m2.kws_2 <- as.matrix(idf_2)
# cluster terms
distMatrix.kws_2 <- dist(scale(m2.kws_2))
fit.kws_2 <- hclust(distMatrix.kws_2, method = "ward.D")
plot(fit.kws_2)
rect.hclust(fit.kws_2, k = 5) # cut tree into 3 clusters 

###

m3.kws <- t(m2.kws_2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 3 # number of clusters
kmeansResult <- kmeans(m3.kws, k)
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

# partitioning around medoids with estimation of number of cluster
pamResult.kws <- pamk(m3.kws, metric="manhattan")
k <- pamResult.kws$nc # number of clusters identified
pamResult.kws <- pamResult.kws$pamobject

# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult.kws$medoids)[which(pamResult.kws$medoids[i,]==1)], "\n")
}

# plot clustering result
par(mar=c(5.1,4.1,4.1,2.1))
layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pamResult.kws, col.p = pamResult.kws$clustering)
layout(matrix(1)) # change back to one graph per page


#Topic Modelling

rowTotals.kws <- apply(dtm.kws, 1, sum) #find the sum of words in each Document
dtm.new.kws   <- dtm.kws[rowTotals.kws> 0, ] #remove all docs without words
(empty.docs.kws = as.numeric(names(rowTotals.kws[rowTotals.kws==0]))) # showing the docs without words
#this is useful to keep a 1:1 correspondence between the dtm and the corpus:
empty.rows.kws <- dtm.kws[rowTotals.kws == 0, ]$dimnames[1][[1]]
myCorpus.kws <- myCorpus.kws[-as.numeric(empty.rows.kws)]

lda.kws <- LDA(dtm.new.kws, k = 8) # find 8 topics
term.kws <- terms(lda.kws, 7) # first 7 terms of every topic
(term.kws <- apply(term.kws, MARGIN = 2, paste, collapse = ", "))


## add date vars experience or seniority?

topics.kws <- topics(lda.kws, 1) # 1st topic identified for every document (tweet)
new.kw.wide = kw.wide[-c(empty.docs.kws), ] # remove the tweets that correspond to the empty docs
topics.kws <- data.frame(date=as.IDate(kw.wide$career_start_date), topic=topics.kws)
qplot(date, ..count.., data=topics.kws, geom="density",
      fill=term.kws[topic], main='kws') + scale_fill_discrete("Topics")


# analysis of main complaints in the industry
#http://whatphoneplans.co.uk/the-most-complained-about-mobile-phone-operators-in-the-uk/

# 'java' (if possible, look for locations in the text)
java <- kw.wide[grep('java', kw.wide$keywords), ]
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
oracle <- kw.wide[grep('oracle', kw.wide$keywords), ]
ggplot(signal, aes(reorder_size(employee_type))) + geom_bar(aes(fill=employee_type)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"oracle\" in data")

#billing
billing <- kw.wide[grep('bill', kw.wide$text), ]
ggplot(billing, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"bill\" in tweets")

#contract
contract <- kw.wide[grep('contract', kw.wide$text), ]
ggplot(contract, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"contract\" in tweets")
# findAssocs test with term 'contract':
contractAssoc <- findAssocs(tdm.kws, 'contract', corlimit = 0.1)
contractAssoc
#then do the previous analysis with associated terms 'contract' and 'end'
contract.end <- contract[grep('end', contract$text), ]
ggplot(contract.end, aes(reorder_size(operator))) + geom_bar(aes(fill=operator)) +
  scale_fill_discrete("Operator") + xlab("Operators") + ylab("Count") +
  ggtitle("Count of term \"end\" in tweets containing term \"contract\"")


