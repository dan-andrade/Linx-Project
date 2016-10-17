library(Boruta)
set.seed(2016)

source('C:/Users/Altran/Documents/Linx Project/varlist.R') # varlist custom function

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small_0310.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(22:30), -(34:38), -40)
small_[c(2:4,6,9,13:16,18:21)] <- sapply(small_[c(2:4,6,9,13:16,18:21)], function(x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:16,18:21)] <- sapply(small_[c(2:4,6,9,13:16,18:21)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:16,18:21)] <- sapply(small_[c(2:4,6,9,13:16,18:21)], function(x) gsub('/', '-', x))


# missing values per column

mv.small <- sapply(small_, function(x){sum(is.na(x))/length(x)})*100
mv.small

#select columns with missing value less than 15%
small_noNA <- subset(small_, select = mv.small < 15 )

# mv imputation: replace some mv with 'other' or zeros
small_noNA$dip_1 <- ifelse(is.na(small_noNA$dip_1)==T, 'other', small_noNA$dip_1)
small_noNA$sk_1 <- ifelse(is.na(small_noNA$sk_1)==T, 'other', small_noNA$sk_1)
small_noNA$kw_1 <- ifelse(is.na(small_noNA$kw_1)==T, 'other', small_noNA$kw_1)
small_noNA$kw_2 <- ifelse(is.na(small_noNA$kw_2)==T, 'other', small_noNA$kw_2)
small_noNA$lang_english <- ifelse(is.na(small_noNA$lang_english)==T, 0, small_noNA$lang_english)
small_noNA$lang_french <- ifelse(is.na(small_noNA$lang_french)==T, 0, small_noNA$lang_french)
small_noNA$lang_spanish <- ifelse(is.na(small_noNA$lang_spanish)==T, 0, small_noNA$lang_spanish)
small_noNA$skills_count <- ifelse(is.na(small_noNA$skills_count)==T, 0, small_noNA$skills_count)
small_noNA$keywords_count <- ifelse(is.na(small_noNA$keywords_count)==T, 0, small_noNA$keywords_count)

vars <- names(small_noNA)
small_noNA[sapply(small_noNA, is.character)] <- lapply(small_noNA[sapply(small_noNA, is.character)],
                                                       as.factor) 


boruta.train <- Boruta(is_active~., data = small_noNA, doTrace = 2,
                       maxRuns=1000)
print(boruta.train)

# plot

plot(boruta.train, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# treat tentative vars

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

# plot w/out tentative vars

plot(final.boruta, xlab = "", xaxt = "n")
lz.final <- lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz.final) <- colnames(final.boruta$ImpHistory)
Labels.final <- sort(sapply(lz.final,median))
axis(side = 1,las=2,labels = names(Labels.final),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

# list final selected vars
getSelectedAttributes(final.boruta, withTentative = F)

# create df
boruta.df <- attStats(final.boruta)
orderBy(~decision, boruta.df)
