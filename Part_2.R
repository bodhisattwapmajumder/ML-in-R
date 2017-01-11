-----------------------------------------------------------------------------------------------------
# The code has been written for the winter project guided by Latenview Analytics as a part of       #
# data modelling by Group 4                                                                         #
# Co-author: Bodhisattwa, Somya and Faichali                                                        #
-----------------------------------------------------------------------------------------------------

setwd("C:/Users/Bodhisattya/Desktop/PGDBA/Latentview Winter/codes/part 2")
library(ggplot2)
data=read.csv("data_new.csv")
library("caTools")

set.seed(88) #this initializes the random number generator, 
#this ensures the random split is same for all the members

split=sample.split(data$Dependent.Company.Status, SplitRatio=0.7) # randomly splits the sample in the ratio 70:30,
# we keep 70% (TRUE) for training and 30% (FALSE) for testing
# above function randomly splits dataset on the basis of 1st argument but 
# also ensures that outcome variable is well balanced in each piece
# We want to do this so that testing set is representative of our training set

table(data$Dependent.Company.Status)
# failure:success ratio is 167:305 which is a bit less 1:2

table(data$Dependent.Company.Status,split)
# we can see that for FALSE i.e for test data, ratio of failure to success is bit less than 1:2
# for TRUE i.e for training data, ratio of failure to success is also bit less than 1:2
# also ratio of no.of TRUEs to no.of FALSEs can be checked if it equals 70:30 or not

dataTrain=subset(data, split==TRUE)
write.csv(dataTrain,"Train_data_group_4.csv")
dataTest=subset(data, split==FALSE)
write.csv(dataTest,"Test_data_group_4.csv")

##############################################################################################
# Logistic Regression (model 3)

#selecting which variables to keep or which variables to remove in the model
vars=colnames(dataTrain)[c(-1,-2,-73,-52,-70,-71,-74)] #model 3
#vars=colnames(dataTrain)[c(3,11,18,20,22,24,25,29,31,32,34,44,50)]
formula=as.formula(paste("Dependent.Company.Status~",paste(vars,collapse="+")))

mylogit=glm(formula, data=dataTrain, family=binomial)
summary(mylogit)

#testing model on training set 
predictTrain=predict(mylogit,type="response")
summary(predictTrain)
tapply(predictTrain,dataTrain$Dependent,mean)
table(dataTrain$Dependent,predictTrain>0.5)

#testing model on test set 
predictTest=predict(mylogit,type="response",newdata=dataTest)
summary(predictTest)
prediction1 <- as.numeric(predictTest > 0.5)
tapply(predictTest,dataTest$Dependent.Company.Status,mean)
confusion_matrix <- table(dataTest$Dependent,predictTest>0.5)
accuracy_dt_lr=sum(diag(confusion_matrix)/nrow(dataTest))
accuracy_dt_lr


# building the ROC curve (Receiver Operator Characteristic)
library("ROCR") 

#AUC for training set 
ROCRpred=prediction(predictTrain,dataTrain$Dependent)#1st arg is predicted,2nd is actual)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred,"auc")@y.values)

#Auc for test set
ROCRpred_lr=prediction(predictTest,dataTest$Dependent)#1st arg is predicted,2nd is actual)
ROCRperf_lr=performance(ROCRpred_lr,"tpr","fpr")
plot(ROCRperf_lr,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred_lr,"auc")@y.values)

######################################################################################################

# Variable selection for tree- based model

#selecting which variables to keep or whicv variables to remove in the model
#vars=colnames(dataTrain)[c(3,11,18,20,22,24,25,29,31,32,34,44,50)]
vars=colnames(dataTrain)[c(4:74)]
formula=as.formula(paste("Dependent.Company.Status~",paste(vars,collapse="+")))

######################################################################################################################

# Decision Tree (CART)
# Model using rpart

library(rpart)
library(rpart.plot)
mydt=rpart(formula,data=dataTrain, method="class",cp=0.03)
# minbucket sets the minimum number of observations that are required in any 
# terminal or leaf node. 
# method="class" tells to build classification instead of regression

#meanvar(mydt, log = 'xy')
#rsq.rpart(mydt)
#printcp(mydt)
#plotcp(mydt)
#plot(mydt, uniform=TRUE,main="Classification Tree")
#text(mydt, use.n=TRUE, all=TRUE, cex=.8)
#prp(mydt)

# plot the tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(mydt)

# Accuracy vs Threshold
predictTrain_dt=predict(mydt,newdata=dataTrain)[,2]
accuracy_dt=c()
for( threshold in seq(0,1,0.01)){
  t=table(dataTrain$Dependent.Company.Status,predictTrain_dt > threshold)
  accuracy_dt=append(accuracy_dt,sum(diag(t)/nrow(dataTrain)))
}
accuracy_dt <- as.data.frame(cbind(accuracy_dt, seq(0,1,0.01)))
plot(accuracy_dt[,2],accuracy_dt[,1], xlab ="threshold", ylab="accuracy", main ="Accuracy vs Threshold curve for Decision Tree",type="l", cex.main=0.9)

# test accuracy 
predictTest_dt=predict(mydt,newdata=dataTest)[,2]
confusion_matrix=table(dataTest$Dependent.Company.Status,predictTest_dt > 0.5)
confusion_matrix
accuracy_test_dt <- sum(diag(confusion_matrix)/nrow(dataTest))

hist(predictTest_dt, xlab = "Prediction score", main = "Histogram of prediction for Decision Tree")

# to calculate AUC
library(ROCR)
ROCRpred_dt=prediction(predictTest_dt,dataTest$Dependent.Company.Status)
ROCRperf_dt=performance(ROCRpred_dt,"tpr","fpr")
#ROCRperf_dt <- performance(ROCRpred_dt, "sens", "spec")

plot(ROCRperf_dt,colorize=TRUE, ylab= "Sensitivity (True Positive Rate)", xlab = "1- Specificity (False Positive Rate)",main= "ROC curve for Decision Tree", cex.main=1)
#abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc_dt=as.numeric(performance(ROCRpred_dt, "auc")@y.values) 
auc_dt

# Train AUC

ROCRpred_train_dt=prediction(predictTrain_dt,dataTrain$Dependent.Company.Status)
ROCRperf_train_dt <- performance(ROCRpred_train_dt, "sens", "spec") 
auc_train_dt=as.numeric(performance(ROCRpred_train_dt, "auc")@y.values) 
auc_train_dt

###########################################################################################


# Cross Vaildation using rpart

library(caret)
library(e1071)
#now we start defining cross validation
#first define how many folds we want
numFolds=trainControl(method="cv",number=15)#cv for cross validation,num for folds
# tried with number=5,10,15,20 cp value is around 0.03-0.04 in all cases
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(formula,data=dataTrain,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
mydt_cv=rpart(formula,data=dataTrain, method="class",cp=0.03)#cp obtained from the above model
predictTest=predict(mydt_cv,newdata=dataTest,type="class")
t=table(dataTest$Dependent.Company.Status,predictTest)
t
accuracy_dt_cv=sum(diag(t))/nrow(dataTest)
accuracy_dt_cv

###############################################################################################


# Conditional Inference Trees

library(party)
myctree=ctree(formula,data=dataTrain)
plot(myctree, main="Conditional Inference Tree", cex=0.5)
predictTest_ct <- c()
for (i in seq(1,nrow(dataTest),1)){ 
predictTest_ct=append(predictTest_ct,predict(myctree,newdata=dataTest,type="prob")[[i]][2])
} 

ROCRpred_ct=prediction(predictTest_ct,dataTest$Dependent)#1st arg is predicted,2nd is actual)
ROCRperf_ct=performance(ROCRpred_ct,"tpr","fpr")

prediction_ct <- as.numeric(predictTest_ct > 0.5)
t=table(dataTest$Dependent.Company.Status,prediction_ct)
t
accuracy_ct=sum(diag(t)/nrow(dataTest))
accuracy_ct

####################################FINAL MODEL (recommended to use) ###########################################################

# Random Forest

library(randomForest)
dataTrain$Dependent.Company.Status=as.factor(dataTrain$Dependent.Company.Status)
dataTest$Dependent.Company.Status=as.factor(dataTest$Dependent.Company.Status)
#nodesize is like minbucket in rpart() for CART
#we can use trees for regression and classification. For CART, we use method argument
#to specify regression or classification. In Random Forest, there is no method argument.
#Classification means we should store output as levels(or factors). So, if we want to do 
#classification in Random Forest, our response or outcome variable should be factor.
myrf=randomForest(formula,data=dataTrain)
#nodesize is the minimum size of terminal nodes

importance <- importance(myrf)
write.csv(importance,"importance_rf.csv")
importance(myrf, type = 1)
varImpPlot(myrf, main = "Importance plot for Random Forest", cex = 0.5)

# Accuracy vs Threshold
predictTrain_rf=predict(myrf,newdata=dataTrain, type = "prob")[,2]
accuracy_rf=c()
for( threshold in seq(0,1,0.01)){
  t=table(dataTrain$Dependent.Company.Status,predictTrain_rf > threshold)
  accuracy_rf=append(accuracy_rf,sum(diag(t)/nrow(dataTrain)))
}
accuracy_rf
accuracy_rf <- as.data.frame(cbind(accuracy_rf, seq(0,1,0.01)))
plot(accuracy_rf[,2],accuracy_rf[,1], xlab ="threshold", ylab="accuracy", main ="Accuracy vs Threshold curve for Random Forest",type="l", cex.main=0.9)

predictTest_rf=predict(myrf,newdata=dataTest, type = "prob")[,2]
confusion_matrix_rf=table(dataTest$Dependent.Company.Status,predictTest_rf > 0.5)
confusion_matrix_rf
accuracy_test_rf <- sum(diag(confusion_matrix_rf)/nrow(dataTest))
accuracy_test_rf

hist(predictTest_rf, xlab = "Prediction score", main = "Histogram of prediction for Random Forest")

# to calculate AUC
library(ROCR)
ROCRpred_rf=prediction(predictTest_rf,dataTest$Dependent.Company.Status)
ROCRperf_rf=performance(ROCRpred_rf,"tpr","fpr")
#ROCRperf_rf <- performaROCRpred2red_rf, "sens", "spec")

plot(ROCRperf_rf,colorize=TRUE, ylab= "Sensitivity (True Positive Rate)", xlab = "1- Specificity (False Positive Rate)",main= "ROC curve for Random Forest", cex.main=1)
#abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc_rf=as.numeric(performance(ROCRpred_rf, "auc")@y.values) 
auc_rf

# Train AUC

ROCRpred_train_rf=prediction(predictTrain_rf,dataTrain$Dependent.Company.Status)
ROCRperf_train_rf <- performance(ROCRpred_train_rf, "sens", "spec") 
auc_train_rf=as.numeric(performance(ROCRpred_train_rf, "auc")@y.values) 
auc_train_rf

#the accuracy can vary if we run the randomForest() again because the
#the tree creation is a random process and due to randomness the accuracy may differ each time

#plot(myrf)
#varUsed(myrf)

################################################################################################
# XGBoost

library(xgboost)

# XGB Dense matrix creation
dtrain <- xgb.DMatrix(data = (data.matrix(dataTrain[,c(-1,-2,-3)])), label = (as.numeric(dataTrain$Dependent.Company.Status)-1))
dtest <- xgb.DMatrix(data = (data.matrix(dataTest[,c(-1,-2,-3,-73)])), label = (as.numeric(dataTest$Dependent.Company.Status)-1))

# Model
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 0.3, nthread = 2, nround = 100, objective = "binary:logistic")
#bstDMatrix_L1 <- xgboost(data = dtrain, max.depth = 2, eta = 0.3, nthread = 2, nround = 500, objective = "binary:logistic", alpha =1)

#bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=100, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")
#rf <- xgboost(data = (data.matrix(dataTrain[,c(-1,-2,-3)])), label = (as.numeric(dataTrain$Dependent.Company.Status)-1), max.depth = 4, num_parallel_tree = 10000, subsample = 0.3, colsample_bytree =0.2, nround = 1, objective = "binary:logistic")

# Prediction
predictTest_xgb <- predict(bstDMatrix, data.matrix(dataTest[,c(-1,-2,-3)]))
prediction_xgb <- as.numeric(predictTest_xgb > 0.47)
tapply(predictTest_xgb,dataTest$Dependent.Company.Status,mean)
table(dataTest$Dependent,predictTest_xgb>0.47)
#prediction_final <- max(pred,predictTest)
#prediction_score <- rowMeans(prediction_final)
err <- mean(as.numeric(predictTest_xgb > 0.47) != (as.numeric(dataTest$Dependent.Company.Status)-1))
print(paste("test-accuracy=", 1-err))


predictTrain_xgb <- predict(bstDMatrix, data.matrix(dataTrain[,c(-1,-2,-3)]))
accuracy_xgb=c()
for( threshold in seq(0,1,0.01)){
  t=table(dataTrain$Dependent.Company.Status,predictTrain_xgb > threshold)
  accuracy_xgb=append(accuracy_xgb,sum(diag(t)/nrow(dataTrain)))
}
accuracy_xgb <- as.data.frame(cbind(accuracy_xgb, seq(0,1,0.01)))
plot(accuracy_xgb[,2],accuracy_xgb[,1], xlab ="threshold", ylab="accuracy", main ="Accuracy vs Threshold curve for XGBoost",type="l", cex.main=0.9)

hist(predictTest_xgb, xlab = "Prediction score", main = "Histogram of prediction for XGBoost")

#AUC for test

library("ROCR")

ROCRpred_xgb=prediction(predictTest_xgb,(as.numeric(dataTest$Dependent.Company.Status)-1))#1st arg is predicted,2nd is actual)
ROCRperf_xgb=performance(ROCRpred_xgb,"tpr","fpr")
plot(ROCRperf_xgb,colorize=TRUE, ylab= "Sensitivity (True Positive Rate)", xlab = "1- Specificity (False Positive Rate)",main= "ROC curve for XGBoost", cex.main=1)
#plot(ROCRperf3,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred_xgb,"auc")@y.values)

# train AUC

ROCRpred_xgb=prediction(predictTrain_xgb,(as.numeric(dataTrain$Dependent.Company.Status)-1))
ROCRperf_xgb <- performance(ROCRpred_xgb, "tpr", "fpr") 
plot(ROCRperf_xgb,colorize=TRUE, ylab= "Sensitivity (True Positive Rate)", xlab = "1- Specificity (False Positive Rate)",main= "ROC curve for XGBoost", cex.main=1)
auc_train_xgb=as.numeric(performance(ROCRpred_xgb, "auc")@y.values) 
auc_train_xgb

# Importance matrix

library(ggplot2)
importance_matrix <- xgb.importance(model = bstDMatrix)
#write.csv(importance_matrix, "importance_xgb.csv")
importance <- xgb.importance(feature_names = colnames(dataTrain[,c(-1,-2,-3)]), model = bstDMatrix)
write.csv(importance, "importance_name_xgb.csv")
print(importance)
xgb.plot.importance(importance_matrix = importance)

################################################################################################

#ROC curve plot for all models


plot(ROCRperf_lr,ylab= "Sensitivity (True Positive Rate)", xlab = "1- Specificity (False Positive Rate)",main= "ROC curves for different models", cex.main=1)
plot(ROCRperf_dt, add = TRUE, col="blue")
plot(ROCRperf_ct, add = TRUE, col="green")
plot(ROCRperf_rf, add = TRUE, col="red")
plot(ROCRperf_xgb, add = TRUE, col="magenta")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend("bottomright", c("Logistic Regression","Decision Tree","Conditional Tree","Random Forest","XGBoost","Random coin toss"),border="black",lty = 1, col=c("black","blue","green","red","magenta","gray"), bty = 'n', cex= .75)
