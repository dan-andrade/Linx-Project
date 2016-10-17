library(mlbench)
library(caret)

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


# prepare training scheme
cntrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
smp2 <- smp_all_noNA[, -26] # taking out mng NICO

varNames <- names(smp2)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("not_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form.smp2 <- as.formula(paste("not_active", varNames1, sep = " ~ "))

smp2$not_active <- as.factor(smp2$not_active)
modelz.rf <- caret::train(rf.form.smp2, data=smp2, method="rf", preProcess="scale", trControl=cntrl)
# estimate variable importance
importancez.rf <- varImp(modelz.rf, scale=FALSE)
# summarize importance
print(importancez.rf)
# plot importance
plot(importancez.rf)

imp.df <- as.data.frame(importancez.rf$importance)
imp <- orderBy(~-Overall, imp.df)
load('C:/Users/Altran/Desktop/BD/29-08/R files/imp.RData')
save(imp, file='C:/Users/Altran/Desktop/BD/29-08/R files/imp.RData', ascii=T)

###########


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
smp2 <- smp_all_noNA[, -26] # taking out mng NICO
results2 <- rfe(smp2[,-45], smp2[,45], sizes=c(1:8), rfeControl=control,
                rerank=T,
                method='cv')
# summarize the results
print(results2)
# list the chosen features
predictors(results2)
# plot the results
plot(results2, type=c("g", "o"))

vnames <- predictors(results2)[1:8]
