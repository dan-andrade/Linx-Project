library(randomForest)
library(dummies)
library(caret)
library(dplyr)
library(caTools)

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


# sample split
spl2 <- sample.split(smp_all_, SplitRatio = 0.7)
train2 <- subset(smp_all_, spl2 == TRUE)
train2$is_active <- as.factor(train2$is_active)
test2 <- subset(smp_all_, spl2 == FALSE)
test2$is_active <- as.factor(test2$is_active)

#build formula to un-imbalance the dataset
varNames <- names(train2)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("is_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("is_active", varNames1, sep = " ~ "))

#un-imbalance the dataset
bal.train <- SMOTE(rf.form, train2, perc.over = 400, k = 5)

#set tuning parameters
control <- trainControl(method = "cv", number = 5)

#random forest model
rf_model <- train(is_active ~ ., data = bal.train, method = "parRF", trControl =  control, prox = TRUE, allowParallel = TRUE)

#check optimal parameters
print(rf_model)

#random forest model
bestmtry <- rf_model$finalModel$mtry
forest_model <- randomForest(is_active ~ ., data = bal.train, mtry = bestmtry, ntree = 1000)
print(forest_model)
varImpPlot(forest_model)
