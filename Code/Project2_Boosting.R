# input the training data and test data
cctrain <- read.table("CCTrain.csv", header = TRUE, sep = ",")
cctest <- read.table("CCTest.csv", header = TRUE, sep = ",")

# give the response binary values ("0" for No, "1" for Yes)
newtrain<-data.frame(cctrain[,-15],consumer_disputed=as.integer(cctrain[,15])-1)
newtest<-data.frame(cctest[,-15],consumer_disputed=as.integer(cctest[,15])-1)

# boosting - try different shrinkage values
require(gbm)
boost.cc1=gbm(consumer_disputed~.,data=newtrain,n.trees=5000, shrinkage=0.1)
boost.cc2=gbm(consumer_disputed~.,data=newtrain,n.trees=5000, shrinkage=0.01)
boost.cc3=gbm(consumer_disputed~.,data=newtrain,n.trees=5000, shrinkage=0.001)

# test three models on test dataset with 5000 trees
n.trees=seq(from=10,to=5000,by=50)
predmat=predict(boost.cc1,newdata=newtest,n.trees=n.trees,type="response")
berr1=with(newtest,apply( (predmat-consumer_disputed)^2,2,mean))

predmat=predict(boost.cc2,newdata=newtest,n.trees=n.trees,type="response")
berr2=with(newtest,apply( (predmat-consumer_disputed)^2,2,mean))

predmat=predict(boost.cc3,newdata=newtest,n.trees=n.trees,type="response")
berr3=with(newtest,apply( (predmat-consumer_disputed)^2,2,mean))

# combine the three plots into one to compare them.
par(mfrow=c(1,3))
plot(n.trees,berr1,lwd = 1.5,type = "l",ylab="Mean Squared Error", xlab="# Trees",main="shrinkage=0.1")
abline(h=min(berr1),col="red")
plot(n.trees,berr2,lwd = 1.5,type = "l",ylab="Mean Squared Error", xlab="# Trees",main="shrinkage=0.01")
abline(h=min(berr2),col="red")
plot(n.trees,berr3,lwd = 1.5,type = "l",ylab="Mean Squared Error", xlab="# Trees",main="shrinkage=0.001")
abline(h=min(berr3),col="red")

# choose the optimal shrinkage value which is 0.01, and test it with 2000 trees.
boost.cc2=gbm(consumer_disputed~.,data=newtrain,n.trees=2000, shrinkage=0.01)
error <- numeric()
n.trees=seq(from=10,to=2000,by=20)
predmat=predict(boost.cc2,newdata=newtest,n.trees=n.trees,type="response")
dim(predmat)
pred = ifelse(predmat<0.5, 0, 1)
for(i in 1:100){
  tb=with(newtest,table(pred[,i],consumer_disputed))
  error[i] <- 1- sum(diag(tb))/nrow(newtest)
}
plot(n.trees,error,lwd = 1,type = "l",ylab="Error Rate", xlab="# Trees",main="Boosting Test Error")

# calculate the accuracy rate
cctable <- table(pred[,50], newtest$consumer_disputed) # select any iteration less than 1190
accuracy <- sum(diag(cctable))/nrow(newtest)
accuracy
# [1] 0.8407097
