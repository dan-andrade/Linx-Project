# from https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/

## to solve issues: https://github.com/mlr-org/mlr/issues/441


Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101') # avoiding JAVA error
library(caTools)
library(mlr)
library(FSelector)
library(randomForest)
library(ROSE)

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
data.rose <- ROSE(is_active ~ ., data = small_, seed = 1)$data
table(data.rose$is_active)

# sample split
spl <- sample.split(data.rose, SplitRatio = 0.7)
train <- subset(data.rose, spl == TRUE)
train$is_active <- as.factor(train$is_active)
test <- subset(data.rose, spl == FALSE)


# ML

#create a task
train$is_active <- as.factor(train$is_active)
test$is_active <- as.factor(test$is_active)
trainTask <- makeClassifTask(data = train, target = "is_active")
testTask <- makeClassifTask(data = test, target = "is_active")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

#drop vars
# trainTask <- dropFeatures(task = trainTask,features = c("employee_id","name"))

#feature importance
im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
## if Java error: Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
plotFilterValues(im_feat,n.show = 20)
#shiny app:
plotFilterValuesGGVIS(im_feat)



### the same, but including NAs and creating dummies for the missings

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

small_miss <- small_

# sample spl_missit
spl_miss <- sample.split(small_miss, SplitRatio = 0.7)
train_miss <- subset(small_miss, spl_miss == TRUE)
test_miss <- subset(small_miss, spl_miss == FALSE)


#It also creates new dummy variables for missing values

train_miss[sapply(train_miss, is.character)] <- lapply(train_miss[sapply(train_miss, is.character)],
                                                       as.factor) 
test_miss[sapply(test_miss, is.character)] <- lapply(test_miss[sapply(test_miss, is.character)],
                                                     as.factor) 
train_miss[c(10:12, 21)] <- sapply(train_miss[c(10:12, 21)], function(x) as.integer(x))
test_miss[c(10:12, 21)] <- sapply(test_miss[c(10:12, 21)], function(x) as.integer(x))

imp <- impute(train_miss, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
imp1 <- impute(test_miss, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")

imp_train_miss <- imp$data
imp_test_miss <- imp1$data


# ML with NAs

#create a task
imp_train_miss$is_active <- as.factor(imp_train_miss$is_active)
imp_test_miss$is_active <- as.factor(imp_test_miss$is_active)
trainTask_miss <- makeClassifTask(data = imp_train_miss, target = "is_active")
testTask_miss <- makeClassifTask(data = imp_test_miss, target = "is_active")

#normalize the variables
trainTask_miss <- normalizeFeatures(trainTask_miss,method = "standardize")
testTask_miss <- normalizeFeatures(testTask_miss,method = "standardize")

#drop vars
# trainTask_miss <- dropFeatures(task = trainTask_miss,features = c("employee_id","name"))

#feature importance
im_feat_miss <- generateFilterValuesData(trainTask_miss, method = c("information.gain","chi.squared"))
## if Java error: Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
plotFilterValues(im_feat_miss,n.show = 20)
#shiny app:
plotFilterValuesGGVIS(im_feat_miss)




#######
##With MLR, we can choose & set algorithms using makeLearner. This learner will train on trainTask and try to make predictions on testTask

#load Quadratic Discriminant Analysis (QDA) 
qda.learner <- makeLearner("classif.qda", predict.type = "response")

#train model
qmodel <- train(qda.learner, trainTask)

#predict on test data
qpredict <- predict(qmodel, testTask)

#create submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = qpredict$data$response)