library(rpart)
library(e1071)
library(rpart.plot)
library(caret)
library(Metrics)

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

vars <- names(small_)
small_[sapply(small_, is.character)] <- lapply(small_[sapply(small_, is.character)],
                                               as.factor) 
if (sum(is.na(small_[vars]))) small_[vars] <- na.roughfix(small_[vars]) #impute by median/mode (randomForest)

# sample split
spl <- sample.split(small_, SplitRatio = 0.7)
train <- subset(small_, spl == TRUE)
test <- subset(small_, spl == FALSE)


#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
tree_model <- train(is_active ~ ., data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)

main_tree <- rpart(is_active ~ ., data = train,
                   control = rpart.control(cp=0.5)) # cp w/ min RMSE
prp(main_tree)

#check the RMSE
pre_score <- predict(main_tree, type = "vector")
# rmse(train$is_active, pre_score) # for numeric only
