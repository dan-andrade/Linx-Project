#CART Model

library(rpart)

library(rpart.plot)

load('C:/Users/Altran/Desktop/BD/29-08/R files/small_noNA.RData')


#check classes distribution
prop.table(table(small_$is_active))

#balance the data
data.rose.sm <- ROSE(is_active ~ ., data = small_, seed = 1)$data
table(data.rose.sm$is_active)

# sample split
spl2_ <- sample.split(data.rose.sm, SplitRatio = 0.7)
train2_ <- subset(data.rose.sm, spl2_ == TRUE)
test2_ <- subset(data.rose.sm, spl2_ == FALSE)



#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
tree_model <- caret::train(is_active ~ ., data = train2_, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)

Tree = rpart(is_active ~ ., data = train2_, method='class',
             maxdepth=3,
             minsplit=20,
             control = rpart.control(cp=0.01, maxdepth=5,)) # lower cp
print(Tree)
prp(Tree)

# accuracy

PredictCART = predict(Tree, newdata = test2_, type = "class")

table(test2_$is_active, PredictCART)

(118+138)/(nrow(test2_)) # 82%

# Sensitivity 

138/(14+138) # 91%

# AUC

library(ROCR)

predictTestCART = predict(Tree, newdata = test2_)

predictTestCART = predictTestCART[,2]

#Compute the AUC:

ROCRCART = prediction(predictTestCART, test2_$is_active)

as.numeric(performance(ROCRCART, "auc")@y.values) # 15%
