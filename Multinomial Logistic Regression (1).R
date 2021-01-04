library(csvread)
library(caret)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ROCR)
library(nnet)
library(pROC)


data=read.csv("~/Desktop/dataTraining.csv")
dataUp=read.csv("~/Desktop/dataUpTraining.csv")
dataDown=read.csv("~/Desktop/dataDownTraining.csv")

names(data)
head(data)

#drop health
responseVar=c('health')
trainSetPreds=data[,!names(data) %in% responseVar]
names(trainSetPreds)
length(trainSetPreds)

#Do the same in Up and Down sampled sets
trainSetPredsUp=dataUp[,!names(dataUp)%in%responseVar]
trainSetPredsDown=dataDown[,!names(dataDown)%in%responseVar]

#pre-process in all three sets
preProcValues=preProcess(trainSetPreds,method=c("center","scale"))
train_processed=predict(preProcValues,trainSetPreds)
preProcValuesUp=preProcess(trainSetPredsUp,method=c("center","scale"))
preProcValuesDown=preProcess(trainSetPredsDown,method=c("center","scale"))
train_processedUp=predict(preProcValuesUp,trainSetPredsUp)
train_processedDown=predict(preProcValuesDown,trainSetPredsDown)

#dummy the categorical vars
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
str(train_transformed)

dmyUp=dummyVars(" ~ .",data=train_processedUp,fullRank=T)
dmyDown=dummyVars(" ~ .",data=train_processedDown,fullRank=T)
train_transformedUp=data.frame(predict(dmyUp,newdata=train_processedUp))
train_transformedDown=data.frame(predict(dmyDown,newdata=train_processedDown))

#add the categorical version of health back in
train_transformed$health = data$health
train_transformedUp$health=dataUp$health
train_transformedDown$health=dataDown$health

#split the data to get my own test set to validate the accuracy
index <- createDataPartition(train_transformed$health, p=0.80, list=FALSE)
trainSet <- train_transformed[index,]
testSet <- train_transformed[-index,]

UpSet=train_transformedUp
DownSet=train_transformedDown

#run Multi-class Logistic Regression model on training set
lrm=nnet::multinom(health~.,data=trainSet)
summary(lrm)

#Prediction on test set
lrmPred=lrm%>%predict(testSet)
summary(lrmPred)

#Confusion Matrix on original train/test set
ct=table(testSet$health,lrmPred) 
confusionMatrix(ct)

#multiclass roc on train/test set
predictions=predict(lrm,newdata=testSet,type="prob")
multiclass.roc(testSet$health,predictions)

#Variable Importance Plot
lrm_imp=varImp(lrm)
plot(lrm_imp)

#Evaluation of Original Training Set Model (ROC Curve, AUC)
testSet=cbind(testSet,lrmPred)
testSet$labelG=(ifelse(testSet$health=="Good",1,0))
testSet$labelF=(ifelse(testSet$health=="Fair",1,0))
testSet$labelP=(ifelse(testSet$health=="Poor",1,0))

#Evaluation for Good v. Fair/Poor
lrmgood=predictions[,2]
lrmscoresG=prediction(predictions=lrmgood,labels=testSet$labelG)
lrmperfG=performance(lrmscoresG,"tpr","fpr")

#Evaluation for Fair v. Good/Poor
lrmfair=predictions[,1]
lrmscoresF=prediction(predictions=lrmfair,labels=testSet$labelF)
lrmperfF=performance(lrmscoresF,"tpr","fpr")

#Evaluation for Poor v. Good/Fair
lrmpoor=predictions[,3]
lrmscoresP=prediction(predictions=lrmpoor,labels=testSet$labelP)
lrmperfP=performance(lrmscoresP,"tpr","fpr")

#Plot ROC curve (Good v. Fair/Poor)
plot(lrmperfG,
      main="ROC Curve for LR Model: One (Good) vs. All (Fair/Poor)",
      xlab="1 - Specificity: False Positive Rate",
      ylab="Sensitivity: True Positive Rate",
      col="darkblue",  lwd = 3)
 abline(0,1, lty = 300, col = "green",  lwd = 3)
 grid(col="aquamarine")
 
 #AUC value
 lrmaucG=performance(lrmscoresG,"auc")
 as.numeric(lrmaucG@y.values)

#Plot ROC curve (Fair v. Good/Poor)
plot(lrmperfF,
      main="ROC Curve for LR Model: One (Fair) vs. All (Good/Poor)",
      xlab="1 - Specificity: False Positive Rate",
      ylab="Sensitivity: True Positive Rate",
      col="darkblue",  lwd = 3)
 abline(0,1, lty = 300, col = "green",  lwd = 3)
 grid(col="aquamarine")
 
 #AUC value
 lrmaucF=performance(lrmscoresF,"auc")
 as.numeric(lrmaucF@y.values)

#Plot ROC curve (Poor v. Good/Fair)
plot(lrmperfP,
      main="ROC Curve for LR Model: One (Poor) vs. All (Good/Fair)",
      xlab="1 - Specificity: False Positive Rate",
      ylab="Sensitivity: True Positive Rate",
      col="darkblue",  lwd = 3)
 abline(0,1, lty = 300, col = "green",  lwd = 3)
 grid(col="aquamarine")
 
 #AUC value
 lrmaucP=performance(lrmscoresP,"auc")
 as.numeric(lrmaucP@y.values)



#Prediction on Up set
lrmPredUp=lrm%>%predict(UpSet)
summary(lrmPredUp)

#Confusion Matrix on Up set 
ctUp=table(UpSet$health,lrmPredUp)
confusionMatrix(ctUp)

#Evaluation of Upsampled Set (ROC Curve, AUC)
UpSet=cbind(UpSet,lrmPredUp)
UpSet$labelG=(ifelse(UpSet$health=="Good",1,0))
UpSet$labelF=(ifelse(UpSet$health=="Fair",1,0))
UpSet$labelP=(ifelse(UpSet$health=="Poor",1,0))

#Evaluation for Good v. Fair/Poor
lrmgoodUp=predictionsUp[,2]
lrmscoresUpG=prediction(predictions=lrmgoodUp,labels=UpSet$labelG)
lrmperfUpG=performance(lrmscoresUpG,"tpr","fpr") 
 
 #Evaluation for Fair v. Good/Poor
 lrmfairUp=predictionsUp[,1]
 lrmscoresUpF=prediction(predictions=lrmfairUp,labels=UpSet$labelF)
 lrmperfUpF=performance(lrmscoresUpF,"tpr","fpr")
 
 #Evaluation for Poor v. Good/Fair
 lrmpoorUp=predictionsUp[,3]
 lrmscoresUpP=prediction(predictions=lrmpoorUp,labels=UpSet$labelP)
 lrmperfUpP=performance(lrmscoresUpP,"tpr","fpr")
  
 #Plot ROC curve (one v all) - Upsampled set
plot(lrmperfUp,
      main="ROC Curve for LR Model Upsampled: One (Good) vs. All (Fair/Poor)",
      xlab="1 - Specificity: False Positive Rate",
      ylab="Sensitivity: True Positive Rate",
      col="darkblue",  lwd = 3)
 abline(0,1, lty = 300, col = "green",  lwd = 3)
 grid(col="aquamarine")
 
 #AUC value
 lrmaucUpG=performance(lrmscoresUpG,"auc")
 as.numeric(lrmaucUpG@y.values)
 lrmaucUpF=performance(lrmscoresUpF,"auc")
 as.numeric(lrmaucUpF@y.values)
 lrmaucUpP=performance(lrmscoresUpP,"auc")
 as.numeric(lrmaucUpP@y.values) 


#Prediction on Down set
lrmPredDown=lrm%>%predict(DownSet)
summary(lrmPredDown)

#Confusion Matrix on Down set
ctDown=table(DownSet$health,lrmPredDown)
confusionMatrix(ctDown)

#Evaluation of Downsampled Set (ROC Curve, AUC)
DownSet=cbind(DownSet,lrmPredDown)
DownSet$labelG=(ifelse(DownSet$health=="Good",1,0))
DownSet$labelF=(ifelse(DownSet$health=="Fair",1,0))
DownSet$labelP=(ifelse(DownSet$health=="Poor",1,0))

#Evaluation for Good v. Fair/Poor
lrmgoodDown=predictionsDown[,2]
lrmscoresDownG=prediction(predictions=lrmgoodDown,labels=DownSet$labelG)
lrmperfDownG=performance(lrmscoresDownG,"tpr","fpr") 

#Evaluation for Fair v. Good/Poor
lrmfairDown=predictionsDown[,1]
lrmscoresDownF=prediction(predictions=lrmfairDown,labels=DownSet$labelF)
lrmperfDownF=performance(lrmscoresDownF,"tpr","fpr")  

#Evaluation for Poor v. Good/Fair
lrmpoorDown=predictionsDown[,3]
lrmscoresDownP=prediction(predictions=lrmpoorDown,labels=DownSet$labelP)
lrmperfDownP=performance(lrmscoresDownP,"tpr","fpr") 

#Plot ROC curve (one v all) - Downsampled set
plot(lrmperfDown,
      main="ROC Curve for LR Model Downsampled: One (Good) vs. All (Fair/Poor)",
      xlab="1 - Specificity: False Positive Rate",
      ylab="Sensitivity: True Positive Rate",
      col="darkblue",  lwd = 3)
 abline(0,1, lty = 300, col = "green",  lwd = 3)
 grid(col="aquamarine")
 
 #AUC value
lrmaucDownG=performance(lrmscoresDownG,"auc")
 as.numeric(lrmaucDownG@y.values) 
lrmaucDownF=performance(lrmscoresDownF,"auc")
 as.numeric(lrmaucDownF@y.values) 
lrmaucDownP=performance(lrmscoresDownP,"auc")
 as.numeric(lrmaucDownP@y.values)  
 
