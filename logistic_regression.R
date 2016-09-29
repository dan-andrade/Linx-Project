# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  
library(aod)
library(ggplot2)
library(Rcpp)
library(pscl)
library(randomForest)
library(caTools)
library(dplyr)
library(ROCR)

load('C:/Users/Altran/Desktop/BD/29-08/R files/all.small.RData')

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

train$is_active <- factor(train$is_active)
test$is_active <- factor(test$is_active)

#build formula
varNames <- names(train)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("is_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("is_active", varNames1, sep = " ~ "))

mylogit <- glm(rf.form, data = train, family = binomial)

fitted.results <- stats::predict(mylogit, type='response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
#confusion matrix
table(train$is_active, fitted.results > 0.5)

#ROCR Curve
ROCRpred <- prediction(fitted.results, train$is_active)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

summary(mylogit)

anova(mylogit, test="Chisq")
pR2(mylogit) # McFadden R2 index can be used to assess the model fit


### 

###
# CIs using profiled log-likelihood
confint(mylogit)
# CIs using standard errors
confint.default(mylogit)

#We can test for an overall effect of [var] using the wald.test (is statistically significant?)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

#'We can also test additional hypotheses about the differences in the coefficients for the different levels of rank.
#'multiply one of them by 1, and the other by -1.
#'The other terms in the model are not involved in the test, so they are multiplied by 0.
#'The second line of code below uses L=l to tell R that we wish to base the test on the vector l (rather than using the Terms option as we did above).

# l <- cbind(0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0)
# wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)


# odds ratios only
exp(coef(mylogit))

# odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
