# CMPE239 PROJECT2 ----------------------------------------------------------------------------------------------
##############################################################
install.packages("data.table",repos="http://cran.us.r-project.org")
install.packages("fpc",repos="http://cran.us.r-project.org")
install.packages("neuralnet",repos="http://cran.us.r-project.org")
install.packages("caret",repos="http://cran.us.r-project.org")
install.packages("bnlearn", repos="http://cran.us.r-project.org")

library(data.table)
library(neuralnet)
library(ggplot2)
library(dplyr)

### read all data in

complaints <- read.csv(file="~/SJSU_ClASSES/CMPE239_DataMining/Project2/company_complaints_2016.csv", header=TRUE,sep=",")
CC.Data <- read.csv(file="~/SJSU_ClASSES/CMPE239_DataMining/Project2/CC.Data.csv", header=TRUE,sep=",")
complaints_train <- fread('~/Downloads/CCTrain.csv',header=TRUE, sep=',')
complaints_test <- fread('~/Downloads/CCTest.csv',header=TRUE, sep=',')

complaints_train <- as.data.frame(complaints_train)
complaints_test <- as.data.frame(complaints_test)
complaints_train <- lapply(complaints_train,factor)
complaints_test <- lapply(complaints_test,factor)

## Check data structure
head(complaints_test)
head(complaints_train)
names(complaints_train)

sapply(complaints_train,function(x) length(unique(x)))
sapply(complaints_test,function(x) length(unique(x)))
summary(complaints$issue)
sapply(complaints_train,class)
sapply(complaints_test,class)

complaints_train <- as.data.frame(complaints_train)
complaints_test <- as.data.frame(complaints_test)

## Data exploration

ggplot(complaints %>% group_by(product) %>% summarise(num_products=length(product)),aes(x=reorder(product,num_products),y=num_products))+geom_bar(stat="identity",fill="#53cfff") + xlab("") + ylab("Number of complaints") + coord_flip() + theme_light(base_size=16)

ggplot(complaints %>% group_by(company) %>% summarise(num_company=length(company)),aes(x=reorder(company,num_company),y=num_company))+geom_bar(stat="identity",fill="#53cfff") + xlab("") + ylab("Number of complaints") + coord_flip() + theme_light(base_size=8)


 ## MODELS
 
##################
## Naive Bayes
##################

library(e1071)

nb_complaints <- naiveBayes(complaints_train$consumer_disputed ~.,data= complaints_train)
summary(nb_complaints)
nb_pred <- predict(nb_complaints,complaints_test[,-ncol(complaints_test)])
# nb_pred <- predict(nb_complaints,complaints_test, type="raw")


tab <-table(nb_pred,complaints_test[,ncol(complaints_test)])
tab
sum(tab[row(tab)==col(tab)])/sum(tab)

### 10-fold cross validation
library(caret)
train_control <- trainControl(method='cv',number=10)

#create model
nb_fit <- train(complaints_train[, -ncol(complaints_train)], complaints_train[, ncol(complaints_train)], method = "nb",trControl=train_control)

nb10fold_pred <- predict(nb_fit,complaints_test[,-ncol(complaints_test)])
tab2 <-table(nb10fold_pred,complaints_test[,ncol(complaints_test)])
tab2
sum(tab2[row(tab2)==col(tab2)])/sum(tab2)
print(confusionMatrix(nb10fold_pred,complaints_test[,ncol(complaints_test)]))

###########
## Naive Bayes using Laplace smoothing, with a=1:
###########

nbsmoo_complaints <- naiveBayes(complaints_train$consumer_disputed ~.,data= complaints_train,laplace=1)
summary(nbsmoo_complaints)
nbsmoo_pred <- predict(nbsmoo_complaints,complaints_test[,-ncol(complaints_test)])
#nbsmoo_pred <- predict(nbsmoo_complaints,complaints_test, type="raw")
write.table(nbsmoo_pred,file="NaiveBayesPred.csv",append=FALSE,row.names=FALSE)

tabsmoo <-table(nbsmoo_pred,complaints_test[,ncol(complaints_test)])
tabsmoo
sum(tabsmoo[row(tabsmoo)==col(tabsmoo)])/sum(tabsmoo)

## using Laplace smoothing, with a=0.001,0.01,0.05,1,2:
nbsmoo_complaints2 <- naiveBayes(complaints_train$consumer_disputed ~.,data= complaints_train,laplace=2)
summary(nbsmoo_complaints2)
nbsmoo_pred2 <- predict(nbsmoo_complaints2,complaints_test[,-ncol(complaints_test)])
#nbsmoo_pred <- predict(nbsmoo_complaints,complaints_test, type="raw")
#write.table(nbsmoo_pred,file="NaiveBayesPred.csv",append=FALSE,row.names=FALSE)

tabsmoo2 <-table(nbsmoo_pred2,complaints_test[,ncol(complaints_test)])
tabsmoo2
sum(tabsmoo2[row(tabsmoo2)==col(tabsmoo2)])/sum(tabsmoo2)
print(confusionMatrix(nbsmoo_pred2,complaints_test[,ncol(complaints_test)]))

###########
## Naive Bayes klaR
###########
library(caret)
library(klaR)

split=0.5
for (i in 1:4)
{
split <- split + 0.1
train.index <- createDataPartition(CC.Data$consumer_disputed.,p=split,list=FALSE)
CC.train <- CC.Data[train.index,]
CC.test <- CC.Data[-train.index,]

NBklaR_model <- NaiveBayes(CC.train$consumer_disputed ~.,data= CC.train, fL=0)
x_test <- CC.test[,-ncol(CC.test)]
y_test <- CC.test[,ncol(CC.test)]

NBklaR_pred <- predict(NBklaR_model,x_test)

print(confusionMatrix(NBklaR_pred$class,y_test))
}

###### 10-fold #####   *******

split=0.2
for (i in 1:8)
{
split <- split + 0.1
train.index <- createDataPartition(complaints_train$consumer_disputed.,p=split,list=FALSE)
CC.train <- complaints_train[train.index,]
CC.test <- complaints_train[-train.index,]

train_control <- trainControl(method='cv',number=10)
grid <- expand.grid(.fL=c(0),.usekernel=c(FALSE),.adjust=c(1))
NBklaR_kmodel <- train(complaints_train[,-ncol(CC.train)],complaints_train$consumer_disputed.,trControl = train_control, method="nb",tuneGrid=data.frame(fL=0.1,usekernel=FALSE,adjust=1))

NBklaR_kpred<- predict(NBklaR_kmodel,complaints_test$[,-ncol(complaints_test)])

ktab <-table(NBklaR_kpred,complaints_test$[,ncol(complaints_test)])
print(sum(ktab[row(ktab)==col(ktab)])/sum(ktab))

# print(confusionMatrix(NBklaR_kpred$class,y_test))
}

##############################
##### Tree Augmented Naive Bayes
###############################

### simple naive bayes in bnlearn library 
library(bnlearn)
bn <- naive.bayes(complaints_train,"consumer_disputed.")
bn_predict <- predict(bn,complaints_test)
t <- table(bn_predict,complaints_test[,ncol(complaints_test)])
print(sum(t[row(t)==col(t)])/sum(t))
print(confusionMatrix(bn_predict,complaints_test[,ncol(complaints_test)]))

### Tree Augmented Naive Bayes
tan <-tree.bayes(complaints_train,"consumer_disputed.")
fitted <- bn.fit(tan, complaints_train,method="bayes")
tan_pred <- predict(tan,complaints_test)
t2 <- table(tan_pred,complaints_test[,ncol(complaints_test)])
print(sum(t2[row(t2)==col(t2)])/sum(t2))
print(confusionMatrix(tan_pred,complaints_test[,ncol(complaints_test)]))
write.table(tan_pred,file="tan.csv",row.names=FALSE)
plot(tan,pos= 1,cex=0.5)
