# COMPARE FEAT IMP METHODS (BORUTA VS RF)

library(e1071)
library(ggplot2)
library(caret)
library(pROC)
library(dplyr)
library(randomForest)
library(dummies)
library(data.table)
library(ROSE)
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

smp_all_noNA <- dummy.data.frame(small_noNA, names=names(small_noNA)[c(2:4,6,9,13,15:16,19,20)])


# vars to numeric
smp_all_noNA[sapply(smp_all_noNA, is.factor)] <- lapply(smp_all_noNA[sapply(smp_all_noNA, is.factor)],
                                                        as.numeric)
smp_all_noNA[sapply(smp_all_noNA, is.character)] <- lapply(smp_all_noNA[sapply(smp_all_noNA, is.character)],
                                                           as.numeric)

smp_all_noNA$at_lisbon <- ifelse(smp_all_noNA$at_lisbon==2, 1, 0)
smp_all_noNA$is_active <- ifelse(smp_all_noNA$is_active==1, 1, 0)
setnames(smp_all_noNA, old='is_active', new='not_active')
setnames(smp_all_noNA, old=c("seniority_yrs_cat>10 yrs", "seniority_yrs_cat0-2 yrs",
                             "seniority_yrs_cat3-5 yrs", "seniority_yrs_cat6-10 yrs",
                             "experience_yrs_cat>20", "experience_yrs_cat0-2 yrs",
                             "experience_yrs_cat11-15 yrs", "experience_yrs_cat16-20",
                             "experience_yrs_cat3-5 yrs", "experience_yrs_cat6-10 yrs"),
         new=c("seniority_yrs_cat10_plus", "seniority_yrs_cat0_2",
               "seniority_yrs_cat3_5", "seniority_yrs_cat6_10", "experience_yrs_cat20_plus",
               "experience_yrs_cat0_2", "experience_yrs_cat11_15", "experience_yrs_cat16_20",
               "experience_yrs_cat3_5", "experience_yrs_cat6_10"))



boruta.train_smp <- Boruta(not_active~., data = smp_all_noNA[, -26], doTrace = 2,
                       maxRuns=100)
print(boruta.train_smp)

# plot

plot(boruta.train_smp, xlab = "", xaxt = "n")
lz_smp <- lapply(1:ncol(boruta.train_smp$ImpHistory),function(i)
  boruta.train_smp$ImpHistory[is.finite(boruta.train_smp$ImpHistory[,i]),i])
names(lz_smp) <- colnames(boruta.train_smp$ImpHistory)
Labels_smp <- sort(sapply(lz_smp,median))
axis(side = 1,las=2,labels = names(Labels_smp),
     at = 1:ncol(boruta.train_smp$ImpHistory), cex.axis = 0.7)

# treat tentative vars

final.boruta_smp <- TentativeRoughFix(boruta.train_smp)
print(final.boruta_smp)

# list final selected vars
getSelectedAttributes(final.boruta_smp, withTentative = F)

# create df
boruta.df_smp <- attStats(final.boruta_smp)
head(orderBy(~decision, boruta.df_smp), 15)


############### comparing with RF method

library(caret)
library(randomForest)
set.seed(2016)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

smp.rfe <- smp_all_noNA[, -26]
smp.rfe$not_active <- as.factor(smp.rfe$not_active)
rfe.train <- rfe(smp.rfe[,-45], smp.rfe[,45], sizes=1:12, rfeControl=control)
rfe.train

plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)
head(predictors(rfe.train), 10)
