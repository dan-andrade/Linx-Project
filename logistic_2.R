library(aod)
library(ggplot2)
library(Rcpp)
library(pscl)
library(randomForest)
library(caTools)
library(dplyr)
library(ROCR)
library(DMwR)
set.seed(2016)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

vars <- names(small_)
small_[sapply(small_, is.character)] <- lapply(small_[sapply(small_, is.character)],
                                               as.factor) 
if (sum(is.na(small_[vars]))) small_[vars] <- na.roughfix(small_[vars]) #impute by median/mode (randomForest)


#check classes distribution
prop.table(table(small_$is_active))

#balance the data
data.rose.small <- ROSE(is_active ~ ., data = small_, seed = 1)$data
table(data.rose.small$is_active)

# sample split
spl <- sample.split(data.rose.small, SplitRatio = 0.7)
train <- subset(data.rose.small, spl == TRUE)
train$is_active <- as.factor(train$is_active)
test <- subset(data.rose.small, spl == FALSE)

#build formula
varNames <- names(small_)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("is_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("is_active", varNames1, sep = " ~ "))

# K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(rf.form,  data=train, method="glm", family="binomial",
                 metric = ifelse(is.factor(train_$is_active), "Accuracy", "Kappa"),
                 trControl = ctrl, tuneLength = 5)


pred = predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$is_active)

#accuracy
accuracy.meas(test$is_active, pred)
#ROC curve
roc.curve(test$is_active, pred, plotit = T)
