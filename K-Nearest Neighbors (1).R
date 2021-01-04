setwd("~/Data/Trees/")

library(ROCR)
library(pROC)
library(class)
library(dplyr)
library(caret)
library(ggpubr)
library(ggplot2)

#Train/ Valid split-----------------------------------------------------------------------------
up = read.csv('dataUpTraining.csv')
dfUp <- createDataPartition(up$health, p=0.80, list=FALSE)
trainUp <- up[ dfUp,]
testUp <- up[-dfUp,]

down = read.csv('dataDownTraining.csv')
dfDown <- createDataPartition(down$health, p=0.80, list=FALSE)
trainDown <- down[dfDown,]
testDown <- down[-dfDown,]

data = read.csv('dataTraining.csv')
df <- createDataPartition(data$health, p=0.80, list=FALSE)
train <- data[ df,]
test <- data[-df,]

#Cross-Validation and KNN Model-----------------------------------------------------------------
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
set.seed(3333)
knn_fit <- train(health ~ ., data = train, method = "knn", trControl=trctrl, tuneLength = 20)
#knn_fit <- train(health ~ ., data = trainDown, method = "knn", tuneLength = 5)
summary(knn_fit)

#Validation-------------------------------------------------------------------------------------
knnPredict <- predict(knn_fit, newdata = testDown )
summary(knnPredict)

#Metrics----------------------------------------------------------------------------------------
confusionMatrix(knnPredict, testDown$health )

plot(knn_fit, print.thres = 0.5, type="S")

knnPredict <- predict(knn_fit, newdata = testDown , type="prob")

#ROC and AUC curves-----------------------------------------------------------------------------
knnRocGood <- multiclass.roc(testDown$health,knnPredict[,"Good"], levels = rev(testDown$health))
knnRocGood
plot(knnRocGood, type="S", print.thres= 0.5)

knnRocFair <- multiclass.roc(testDown$health,knnPredict[,"Fair"], levels = rev(testDown$health))
knnRocFair
plot(knnRocFair, type="S", print.thres= 0.5)

knnRocPoor <- multiclass.roc(testDown$health,knnPredict[,"Poor"], levels = rev(testDown$health))
knnRocPoor
plot(knnRocPoor, type="S", print.thres= 0.5)