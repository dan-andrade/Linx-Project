library(mlbench)
library(caret)

# prepare training scheme
cntrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
modelz.rf <- train(rf.form_, data=smp_all_noNA, method="rf", preProcess="scale", trControl=cntrl)
# estimate variable importance
importancez.rf <- varImp(modelz.rf, scale=FALSE)
# summarize importance
print(importancez.rf)
# plot importance
plot(importancez.rf)

imp.df <- as.data.frame(importancez.rf$importance)
imp <- orderBy(~-Overall, imp.df)


###########


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results2 <- rfe(smp_all_noNA[,-46], smp_all_noNA[,46], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results2)
# list the chosen features
predictors(results2)
# plot the results
plot(results2, type=c("g", "o"))