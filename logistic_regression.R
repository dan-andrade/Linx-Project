# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
  
library(aod)
library(ggplot2)
library(Rcpp)
library(pscl)

mydata <- small_

mydata$is_active <- factor(mydata$is_active)

#build formula
varNames <- names(mydata)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("is_active")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
rf.form <- as.formula(paste("is_active", varNames1, sep = " ~ "))

mylogit <- glm(rf.form, data = mydata, family = "binomial")

summary(mylogit)

anova(mylogit, test="Chisq")
pR2(mylogit) # McFadden R2 index can be used to assess the model fit


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
