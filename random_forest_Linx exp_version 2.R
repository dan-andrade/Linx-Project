# linx - Random Forest


library(randomForest)
library(caret)
library(dplyr)

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
test2 <- subset(smp_all_, spl2 == FALSE)



# make a formula

# train2[sapply(train2, is.integer)] <- lapply(train2[sapply(train2, is.integer)], as.factor) 
# train2[sapply(train2, is.character)] <- lapply(train2[sapply(train2, is.character)], as.factor)


### removing duplicated names
train2 <- train2[duplicated(names(train2))==F]
vars <- names(train2)
if (sum(is.na(train2[vars]))) train2[vars] <- na.roughfix(train2[vars]) #impute by median/mode (randomForest)
###


train2$is_active <- as.factor(train2$is_active) # response as a factor (if not, will assume regression)
varNames <- names(train2)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("is_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("is_active", varNames1, sep = " ~ "))


# building the RF

dsc.rf <- randomForest(rf.form, train2, ntree=1000, importance=T)
plot(dsc.rf)

pred_train <- predict(dsc.rf, train2)

#Evaluating - Confusion matrix

confusionMatrix(pred_train, train2$is_active)

# Predict using the test values

pred_test <- predict(dsc.rf, test2)
confusionMatrix(pred_test, test2$is_active)


# Variable Importance Plot
varImpPlot(dsc.rf, sort = T, main="Variable Importance", n.var=5)
# Variable Importance Table
var.imp <- data.frame(importance(dsc.rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$IncNodePurity,decreasing = T),]


# generic predict function can be used for predicting a response variable

train2$predicted.response <- predict(dsc.rf ,train2)




##########

library(randomForest)
library(ROCR)

bestmtry <- tuneRF(train2,train2$is_active, ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) # to get mtry

dsc.rf <-randomForest(train2$is_active~.,data=train2, mtry=bestmtry, ntree=1000, 
                      keep.forest=TRUE, importance=TRUE, test=data$val)

# performance

### TO BE CONCLUDED
dsc.rf.pr = predict(dsc.rf,type="prob",newdata=data$val)[,2]
dsc.rf.pred = prediction(dsc.rf, data$val$income)
dsc.rf.perf = performance(dsc.rf.pred,"tpr","fpr")
plot(dsc.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
### TO BE CONCLUDED