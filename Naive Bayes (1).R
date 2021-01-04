library(dplyr)
library(caret)

setwd("/Users/evan/Documents/OR568_PredictiveAnalytics/Project/")

#pull in training sets up and down
dataUp = read.csv('dataUpTraining.csv')[,-1]
dataDown = read.csv('dataDownTraining.csv')[,-1]

#selecting which data set (Down or Up) to use
trainingData <- dataDown

names(trainingData)
head(trainingData)

#caret help
#??caret
#names(getModelInfo())

#drop health so I just have the predictors
responseVar = c('health')
trainSetPreds = trainingData[,!names(trainingData) %in% responseVar]
names(trainSetPreds)
length(trainSetPreds)

#pre-process the numerical predictors
preProcValues <- preProcess(trainSetPreds, method = c("center","scale"))
train_processed <- predict(preProcValues, trainSetPreds)

#dummy the categorical vars
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
str(train_transformed)

#add the categorical version of health back in
train_transformed$health = trainingData$health

#split the data to get my own test set to validate the accuracy of naive bayes
index <- createDataPartition(train_transformed$health, p=0.80, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]

#feature selection
#control <- rfeControl(functions = rfFuncs,
  #                    method = "repeatedcv",
#                     repeats = 3,
 #                     verbose = FALSE)

predictors<-names(trainSet)[!names(trainSet) %in% responseVar]

#train a naive bayes classifier
nb <- train(health~.,data=trainSet,
            method='naive_bayes')

#predict the test set and build confusion matrix
predictions<-predict.train(object=nb,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet$health)

