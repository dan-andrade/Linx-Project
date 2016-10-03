#svm for 90 points txt file (from Altran's dst Science course)

library(e1071)
library(ggplot2)
library(caret)
library(pROC)
library(dplyr)
library(randomForest)
library(dummies)
set.seed(2016)

source('C:/Users/Altran/Documents/Linx Project/varlist.R') # varlist custom function

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

# missing values per column

mv.small <- sapply(small_, function(x){sum(is.na(x))/length(x)})*100
mv.small
#select columns with missing value less than 15%
small_noNA <- subset(small_, select = mv.small < 15 )

# impute mv

vars <- names(small_noNA)
small_noNA[sapply(small_noNA, is.character)] <- lapply(small_noNA[sapply(small_noNA, is.character)],
                                                   as.factor) 
if (sum(is.na(small_noNA[vars]))) small_noNA[vars] <- na.roughfix(small_noNA[vars]) #impute by median/mode (randomForest)

smp_all_noNA <- dummy.data.frame(small_noNA, names=names(small_noNA)[c(2:4,6,9,13:15)])

dst2 <- smp_all_noNA
dst <- as.data.frame(sapply(dst2[, -46], function(x) as.numeric(x)))
dst$is_active <- dst2$is_active
scd <- as.data.frame(scale(dst[-392]))
scd$L <- as.factor(dst$is_active)
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

svm.tune <- train(x=xtrain[-392],
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
predd <- predict(modell, xtest[,-392])

# Check accuracy:
table(predd, xtest[,392])
# more complete:
confusionMatrix(predict(modell, xtest), xtest$L)

# Plot predicted & missclassified
qplot(experience_yrs, seniority_yrs, colour = as.factor(L), shape = predd, data = xtest)


# compare with Linear SVM

#Train and Tune the SVM
svmLinear.tune <- train(x=xtrain[-392],
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
predd2 <- predict(model2, xtest[,-3])

# Check accuracy:
table(predd2, xtest[,3])
# more complete:
confusionMatrix(predict(model2, xtest), xtest$L)

# Plot predicted & missclassified
qplot(A, B, colour = as.factor(L), shape = predd2, data = xtest)


###

# METHOD 3 (full dstset w/out training and testing sets)


###
# alternatively the traditional interface:
x <- subset(dst, select = -L)
y <- dst$L

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
qplot(A, B, colour = as.factor(L), shape = predd3, data = dst)







