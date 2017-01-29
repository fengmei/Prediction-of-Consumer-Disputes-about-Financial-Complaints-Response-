##
setwd("/Users/XC/Dropbox/SJSU/Courses/Cmpe 239 2016 Spring/Project2/Code")

library(data.table)
library(bit64)
CC.train <- fread("../Data/CCTrain.csv",header = TRUE, stringsAsFactors = TRUE,na.strings = "")
CC.test <- fread("../Data/CCTest.csv",header = TRUE, stringsAsFactors = TRUE,na.strings = "")
CC.train <- as.data.frame(CC.train)
CC.test <- as.data.frame(CC.test)

############ Logistic Regression

fit.lr <- glm(CC.train$`consumer_disputed.` ~ ., family = binomial(logit), data = CC.train) 
summary(fit.lr)

## predict new data, get confusion matrix
lr.test.prob <- predict(fit.lr, newdata = CC.test[, -ncol(CC.test)], type="response")

#pred.logit <- ifelse(lr.test.prob >= 0.5, 1 , 0)

#lr.confusion.1 <- table(pred.logit, CC.test$`consumer_disputed.`)
#accuracy.1 <- sum(diag(lr.confusion.1))/sum(lr.confusion.1)
#accuracy.1

## plot all predicted probabilities
plot(lr.test.prob,pch =20, main= "Predicted Probability",ylab="Probability",xlab = "Testing Observations", cex = 0.5)
abline(0.5, 0, col = "red", lwd = 2, lty = 2)

## plot ROC
library(pROC)

roccurve <- roc(CC.test$consumer_disputed. ~ lr.test.prob)
plot(roccurve,col="red",main="ROC", lwd = 2)

# AREA under the curve is: 0.6963

## predicted accuracy based on different threshold = 0.4, 0,5, 0.6, 0.7, 0.8


pred.logit4 <- ifelse(lr.test.prob >= 0.4, 1 , 0)
lr.confusion.4 <- table(pred.logit4, CC.test$`consumer_disputed.`)
accuracy.4 <- sum(diag(lr.confusion.4))/sum(lr.confusion.4)
accuracy.4

pred.logit5 <- ifelse(lr.test.prob >= 0.5, 1 , 0)
lr.confusion.5 <- table(pred.logit5, CC.test$`consumer_disputed.`)
accuracy.5 <- sum(diag(lr.confusion.5))/sum(lr.confusion.5)
accuracy.5


pred.logit6 <- ifelse(lr.test.prob >= 0.6, 1 , 0)
lr.confusion.6 <- table(pred.logit6, CC.test$`consumer_disputed.`)
accuracy.6 <- sum(diag(lr.confusion.6))/sum(lr.confusion.6)
accuracy.6

pred.logit7 <- ifelse(lr.test.prob >= 0.7, 1 , 0)
lr.confusion.7 <- table(pred.logit7, CC.test$`consumer_disputed.`)
accuracy.7 <- sum(diag(lr.confusion.7))/sum(lr.confusion.7)
accuracy.7

# write.csv(pred.logit7, file = '../Data/lr_results.csv', row.names = F)


pred.logit8 <- ifelse(lr.test.prob >= 0.8, 1 , 0)
lr.confusion.8 <- table(pred.logit8, CC.test$`consumer_disputed.`)
accuracy.8 <- sum(diag(lr.confusion.8))/sum(lr.confusion.8)
accuracy.8

threshold.prob <- c( 0.4, 0.5, 0.6, 0.7, 0.8)
accuracy.all <- c(accuracy.4, accuracy.5, accuracy.6, accuracy.7, accuracy.8)
plot(threshold.prob,accuracy.all, pch = 20, col = "red", lty = 2, ylim = c(0.83, 0.85), type = "b", ylab = "Accuracy", xlab = "threshold")

# best is when 0.7, accuracy 0.8408094
# pred.logit7   No  Yes
# 0 8433 1596
# 1    1    2
table(CC.train$consumer_disputed.)
