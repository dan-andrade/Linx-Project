#svm for 90 points txt file (from Altran's dst Science course)

library(e1071)
library(ggplot2)
library(caret)
library(pROC)
library(dplyr)
library(randomForest)
library(dummies)
library(data.table)
library(ROSE)
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

#check classes distribution
prop.table(table(smp_all_noNA$not_active))

# balance the data
smp_all_noNA$not_active <- as.factor(smp_all_noNA$not_active)
#sample split
spl3 <- sample.split(smp_all_noNA, SplitRatio = 0.9)
smp90 <- subset(smp_all_noNA, spl3 == TRUE)
smp10 <- subset(smp_all_noNA, spl3 == FALSE)

data.rose.smp.noNA <- ROSE(not_active ~ ., data = smp90, seed = 1)$data
table(data.rose.smp.noNA$not_active)


dst2 <- data.rose.smp.noNA
dst <- as.data.frame(sapply(dst2[, -46], function(x) as.numeric(x)))
dst$not_active <- dst2$not_active
# scd <- as.data.frame(scale(dst[-404]))
scd <- dst
scd$L <- as.factor(dst$not_active)
n <- nrow(scd)
ntrain <- n*0.7
tindex <- sample(n,ntrain)
xtrain <- scd[tindex,] #feature variables for training dst
xtest <- scd[-tindex,] #feature variables for test dst

# METHOD 1


# First pass

# 10 fold cross validation
ctr <- trainControl(method="repeatedcv",   # 10fold cross validation
                    repeats=5,		    # do 5 repititions of cv
                    summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                    classProbs=TRUE)

#Train and Tune the SVM

levels(xtrain$L) <- c('one', 'zero') # '0' and '1' not accept by the following train formula
levels(xtest$L) <- c('one', 'zero') 

svm.tune <- train(x=xtrain[-404],
                  y= xtrain$L,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 10,					# 10 values of the cost function
                  metric="ROC",
                  trControl=ctr)
svm.tune
b.sigma <- as.numeric(svm.tune$bestTune[1])
b.gamma <- 1/(2*b.sigma^2)
C <- as.numeric(svm.tune$bestTune[2])

modell  <- svm(L~., data = xtrain, kernel = "radial", 
               gamma = b.gamma, cost = C) 

# test the model
predd <- predict(modell, xtest[,-404])

# Check accuracy:
table(predd, xtest[,404])
# more complete:
confusionMatrix(predict(modell, xtest), xtest$L)

# Plot predicted & missclassified
qplot(experience_yrs, seniority_yrs, colour = as.factor(L), shape = predd, data = xtest)


# compare with Linear SVM

#Train and Tune the SVM
svmLinear.tune <- train(x=xtrain[-404],
                        y= xtrain$L,
                        method = "svmLinear",
                        metric="ROC",
                        trControl=ctr)	

svmLinear.tune

rValues <- resamples(list(Radial=svm.tune,Linear=svmLinear.tune))
rValues$values
summary(rValues)

# comparison between Radial and Linear SVMs
bwplot(rValues,metric="ROC",ylab =c("linear kernel", "radial kernel"))	

## or:
# collect resamples
results <- resamples(list(RadSVM=svm.tune, LinSVM=svmLinear.tune))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


###

# METHOD 2


tuned <- tune.svm(L~., data = xtrain, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

t.gamma <- as.numeric(tuned$best.parameters[1])
C2 <- as.numeric(tuned$best.parameters[2])
model2  <- svm(L~., data = xtrain, kernel = "radial", 
               gamma = t.gamma, cost = C2) 

# test the model
predd2 <- predict(model2, xtest[,-405])

# Check accuracy:
confusionMatrix(predict(model2, xtest), xtest$L)
#ROC curve
roc.curve(xtest$L, predd2, plotit = T)

# Plot predicted & missclassified
qplot(experience_yrs, seniority_yrs, colour = as.factor(L), shape = predd2, data = xtest)


# validate with real data

# test the model

smp10$L <- as.factor(smp10$not_active)
predd3 <- predict(model2, smp10[,-405])

# Check accuracy:
confusionMatrix(predict(model2, smp10), smp10$L)
#ROC curve
roc.curve(smp10$L, predd3, plotit = T)

# Plot predicted & missclassified
qplot(experience_yrs, seniority_yrs, colour = as.factor(L), shape = predd3, data = smp10)


###

# METHOD 3 (full dataset w/out training and testing sets)


###
# alternatively the traditional interface:
x <- subset(dst, select = -is_active)
y <- dst$is_active

tuned2 <- tune.svm(x, y, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned2)

t.gamma2 <- as.numeric(tuned2$best.parameters[1])
C3 <- as.numeric(tuned2$best.parameters[2])
model3  <- svm(x, y, kernel = "radial", 
               gamma = t.gamma2, cost = C2)
summary(model3)

# test with train dst
predd3 <- predict(model3, x)
# (same as:)
predd3 <- fitted(model3)

# Check accuracy:
table(predd3, y)

# Plot predicted & missclassified
qplot(experience_yrs, seniority_yrs, colour = as.factor(is_active), shape = predd3, data = dst)







