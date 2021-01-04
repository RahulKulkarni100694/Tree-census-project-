library(dplyr)
library(caret)
library(e1071)

df = read.csv("C:/Users/RAHUL/Downloads/dataTraining.csv")


require(randomForest)
#train=sample(1:nrow(df),300)


# Split into Train and Validation set
set.seed(100)
train <- sample(nrow(df), 0.80*nrow(df), replace = FALSE)
TrainSet <- df[train,]
ValidSet <- df[-train,]
summary(TrainSet)
summary(ValidSet)



# Create a Random Forest model with default parameters
model1 <- randomForest(health ~ ., data = TrainSet,mtry = 5,  importance = TRUE)
model1



# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
predValid




# classification accuracy
#############*********########################
confusionMatrix(predValid,ValidSet$health)




# To check important variables
importance(model1)        
varImpPlot(model1)  


# 


#tuning parameters
model2 <- randomForest(health ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2


# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
predValid



# classification accuracy
#############*********########################
confusionMatrix(predValid,ValidSet$health)



# To check important variables
importance(model2)        
varImpPlot(model2)   






