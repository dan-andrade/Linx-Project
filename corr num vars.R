library(diplyr)
library(randomForest)
library(caret)
library(mlr)
source('C:/Users/Altran/Documents/Linx Project/varlist.RData')

small_ <- dplyr::select(all.small, -1, -11, -19, -(21:29), -(33:38), -40, -41)
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function (x) gsub("[[:punct:]]", "", x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub(' ', '_', x))
small_[c(2:4,6,9,13:20)] <- sapply(small_[c(2:4,6,9,13:20)], function(x) gsub('/', '-', x))

# missing values per column

mv.small <- sapply(small_, function(x){sum(is.na(x))/length(x)})*100
mv.small
#select columns with missing value less than 5%
small_noNA <- subset(small_, select = mv.small < 5 )

# impute mv

vars <- names(small_noNA)
small_[sapply(small_noNA, is.character)] <- lapply(small_noNA[sapply(small_, is.character)],
                                               as.factor) 
if (sum(is.na(small_noNA[vars]))) small_noNA[vars] <- na.roughfix(small_noNA[vars]) #impute by median/mode (randomForest)



# corr between vars

##

#testing varlist custom function
sapply(small_[ , varlist(small_,
                         type="factor", pattern='_')],
       class) # check the class for the vars

##

# filter out variables with high correlation
#corr set threshold as 0.7

num_var <- varlist(small_, type="numeric")
small_num <- small_[, num_var]
ax <- findCorrelation(x = cor(small_num), cutoff = 0.7)
small_num <- small_num[ , -ax] 

##

####################

#check classes distribution
prop.table(table(small_noNA$is_active))

#balance the data
data.rose_noNA <- ROSE(is_active ~ ., data = small_noNA, seed = 1)$data
table(data.rose_noNA$is_active)

# sample split
spl2 <- sample.split(data.rose_noNA, SplitRatio = 0.7)
train2 <- subset(data.rose_noNA, spl2 == TRUE)
train2$is_active <- as.factor(train2$is_active)
test2 <- subset(data.rose_noNA, spl2 == FALSE)
test2$is_active <- as.factor(test2$is_active)

#create task
train.task <- makeClassifTask(data = train2, target = "is_active")
test.task <- makeClassifTask(data=test2, target = "is_active")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

