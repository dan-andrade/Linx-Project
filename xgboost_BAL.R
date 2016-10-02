library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(randomForest)
library(dummies)
library(caret)
library(dplyr)
library(caTools)
library(ROSE)
set.seed(2016)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)

# small_[sapply(small_, function(x) nlevels(x)==2)] <- lapply(small_[sapply(small_, function(x) nlevels(x)==2)], as.numeric) 
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))


smp_all_ <- dummy.data.frame(small_, names=names(small_)[c(2:4,6,9,13:20)])

vars <- names(smp_all_)
smp_all_[, vars] <- lapply(smp_all_[, vars], as.numeric) 

if (sum(is.na(smp_all_[vars]))) smp_all_[vars] <- na.roughfix(smp_all_[vars]) #impute by median/mode (randomForest)


#check classes distribution
prop.table(table(smp_all_$is_active))

#balance the data
data.rose.smp <- ROSE(is_active ~ ., data = smp_all_, seed = 1)$data
table(data.rose.smp$is_active)

# sample split
spl2 <- sample.split(data.rose.smp, SplitRatio = 0.7)
train2 <- subset(data.rose.smp, spl2 == TRUE)
test2 <- subset(data.rose.smp, spl2 == FALSE)


# xgboost
train3 <- train2
train3$is_active <- ifelse(train3$is_active==1, 1, 0)
#xgb.DMatrix useful for the most advanced features
dtrain <- xgb.DMatrix(data = as.matrix(train3[,-train3$is_active]), label = train3$is_active)
bstSparse <- xgboost(data = dtrain,
                     max.depth = 2,
                     eta = 1,
                     nthread = 2,
                     nround = 2,
                     objective = "binary:logistic",
                     verbose = 2)

#perform the prediction

test3 <- test2
test3$is_active <- ifelse(test3$is_active==1, 1, 0)
pred <- predict(bstSparse, as.matrix(test3[,-test3$is_active]))
# size of the prediction vector
print(length(pred))

#now, we use a regression model to perform a binary classification
#set the rule that if this probability for a specific datum is > 0.5
#then the observation is classified as 1 (or 0 otherwise)
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

#model performance
err <- mean(as.numeric(pred > 0.5) != test3$is_active)
print(paste("test-error=", err))


# predict values in test set
y_pred2 <- predict(bstSparse, data.matrix(test3[,-test3$is_active]))
#accuracy
accuracy.meas(test3$is_active, y_pred2)
#ROC curve
roc.curve(test3$is_active, y_pred2, plotit = F)

###

#feature importance
importance_matrix <- xgb.importance(model = bstSparse)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
#view the trees from a model
xgb.dump(bstSparse, with.stats = T)
xgb.plot.tree(model = bstSparse)


