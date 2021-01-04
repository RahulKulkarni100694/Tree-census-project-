library(dplyr)
library(caret)
library(factoextra)
library(FactoMineR)
library(ClustOfVar)
library(h2o)
library(nnet)
library(caTools)
library(klaR)
library(gbm)
library(SDMTools)
library(kernlab)
library(ROCR)
library(pROC)
library(randomForest)

setwd("~/OR568/Data")

##### Read in Training Data #####
#pull in training sets up and down
trainingU = read.csv('dataUpTraining.csv')
trainingD = read.csv('dataDownTraining.csv')
training = read.csv("dataTraining.csv")
test = read.csv("test.csv")

##### Cross-Validation: Validation-Set Approach ##### FIX
# For original data
rowT<-nrow(training)
trainindex <- sample(rowT, 0.8*rowT, replace=FALSE)
train <- training[trainindex,]
validation <- training[-trainindex,]
xtrain <- dplyr::select(train, -"health")
ytrain <- dplyr::select(train, "health")

# For UpSampled data
rowU<-nrow(trainingU)
trainindexU <- sample(rowU, 0.8*rowU, replace=FALSE)
trainU <- trainingU[trainindexU,]
validationU <- trainingU[-trainindexU,]

# For DownSampled data
rowD<-nrow(trainingD)
trainindexD <- sample(rowD, 0.8*rowD, replace=FALSE)
trainD <- trainingD[trainindexD,]
validationD <- trainingD[-trainindexD,]

##### GBM #####
# GBM on Original Training #####
gbmModel <- gbm(health~., data = train, distribution = "multinomial", n.trees =150, interaction.depth=3, 
                shrinkage=0.1, cv.folds = 3, n.minobsinnode = 10)
gbm.probs <- predict(gbmModel, n.trees=150, newdata = validation, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probs <- apply(gbm.probs, 1, which.max)
p.gbm.probs <- as.data.frame(p.gbm.probs)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probs$p.gbm.probs[p.gbm.probs$p.gbm.probs==1] <- "Fair"
p.gbm.probs$p.gbm.probs[p.gbm.probs$p.gbm.probs==2] <- "Good"
p.gbm.probs$p.gbm.probs[p.gbm.probs$p.gbm.probs==3] <- "Poor"
p.gbm.probs$p.gbm.probs <- as.factor(p.gbm.probs$p.gbm.probs)
# confusion matrix
gbm_matrix <- confusionMatrix(p.gbm.probs$p.gbm.probs, validation$health)
gbm_matrix
# test on test set
gbm.probsT <- predict(gbmModel, n.trees=150, newdata = test, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probsT <- apply(gbm.probsT, 1, which.max)
p.gbm.probsT <- as.data.frame(p.gbm.probsT)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probsT$p.gbm.probsT[p.gbm.probsT$p.gbm.probsT==1] <- "Fair"
p.gbm.probsT$p.gbm.probsT[p.gbm.probsT$p.gbm.probsT==2] <- "Good"
p.gbm.probsT$p.gbm.probsT[p.gbm.probsT$p.gbm.probsT==3] <- "Poor"
p.gbm.probsT$p.gbm.probsT <- as.factor(p.gbm.probsT$p.gbm.probsT)
# confusion matrix
gbm_matrixT <- confusionMatrix(p.gbm.probsT$p.gbm.probsT, test$health)
gbm_matrixT

# GBM on UpSampled data #####
gbmModelU <- gbm(health~., data = trainU, distribution="multinomial", n.trees =150, interaction.depth=3, cv.folds = 3,
                 shrinkage=0.1, n.minobsinnode = 10)
gbm.probsU <- predict(gbmModelU, n.trees=150, newdata = validationU, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probsU <- apply(gbm.probsU, 1, which.max)
p.gbm.probsU <- as.data.frame(p.gbm.probsU)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probsU$p.gbm.probsU[p.gbm.probsU$p.gbm.probsU==1] <- "Fair"
p.gbm.probsU$p.gbm.probsU[p.gbm.probsU$p.gbm.probsU==2] <- "Good"
p.gbm.probsU$p.gbm.probsU[p.gbm.probsU$p.gbm.probsU==3] <- "Poor"
p.gbm.probsU$p.gbm.probsU <- as.factor(p.gbm.probsU$p.gbm.probsU)
# confusion matrix
gbm_matrixU <- confusionMatrix(p.gbm.probsU$p.gbm.probsU, validationU$health)
gbm_matrixU
# test on test set
gbm.probsUT <- predict(gbmModelU, n.trees=150, newdata = test, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probsUT <- apply(gbm.probsUT, 1, which.max)
p.gbm.probsUT <- as.data.frame(p.gbm.probsUT)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probsUT$p.gbm.probsUT[p.gbm.probsUT$p.gbm.probsUT==1] <- "Fair"
p.gbm.probsUT$p.gbm.probsUT[p.gbm.probsUT$p.gbm.probsUT==2] <- "Good"
p.gbm.probsUT$p.gbm.probsUT[p.gbm.probsUT$p.gbm.probsUT==3] <- "Poor"
p.gbm.probsUT$p.gbm.probsUT <- as.factor(p.gbm.probsUT$p.gbm.probsUT)
# confusion matrix
gbm_matrixUT <- confusionMatrix(p.gbm.probsUT$p.gbm.probsUT, test$health)
gbm_matrixUT

# GBM on DownSampled data #####
gbmModelD <- gbm(health~., data = trainD, distribution="multinomial", n.trees =150, cv.folds = 5,
                 interaction.depth=3, shrinkage=0.1, n.minobsinnode = 10)
gbm.probsD <- predict(gbmModelD, n.trees=150, newdata = validationD, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probsD <- apply(gbm.probsD, 1, which.max)
p.gbm.probsD <- as.data.frame(p.gbm.probsD)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probsD$p.gbm.probsD[p.gbm.probsD$p.gbm.probsD==1] <- "Fair"
p.gbm.probsD$p.gbm.probsD[p.gbm.probsD$p.gbm.probsD==2] <- "Good"
p.gbm.probsD$p.gbm.probsD[p.gbm.probsD$p.gbm.probsD==3] <- "Poor"
p.gbm.probsD$p.gbm.probsD <- as.factor(p.gbm.probsD$p.gbm.probsD)
# confusion matrix
gbm_matrixD <- confusionMatrix(p.gbm.probsD$p.gbm.probsD, validationD$health)
gbm_matrixD
# test on test set
gbm.probsDT <- predict(gbmModelD, n.trees=150, newdata = test, type = "response") #gave probabilities
# created data frame which took the max probability for each tree and converted it to a number
p.gbm.probsDT <- apply(gbm.probsDT, 1, which.max)
p.gbm.probsDT <- as.data.frame(p.gbm.probsDT)
# converted the numbers back to their factor levels (1 = Fair, 2 = Good, 3 = Poor; alphabetical)
p.gbm.probsDT$p.gbm.probsDT[p.gbm.probsDT$p.gbm.probsDT==1] <- "Fair"
p.gbm.probsDT$p.gbm.probsDT[p.gbm.probsDT$p.gbm.probsDT==2] <- "Good"
p.gbm.probsDT$p.gbm.probsDT[p.gbm.probsDT$p.gbm.probsDT==3] <- "Poor"
p.gbm.probsDT$p.gbm.probsDT <- as.factor(p.gbm.probsDT$p.gbm.probsDT)
# confusion matrix
gbm_matrixDT <- confusionMatrix(p.gbm.probsDT$p.gbm.probsDT, test$health)
gbm_matrixDT


# Evaluation of Original Training Set Model (ROC Curve, AUC, Lift Chart) #####
test <-cbind(test,gbm.probsT)
test$labelG <- (ifelse(test$health=="Good",1,0))
test$labelF <- (ifelse(test$health=="Fair",1,0))
test$labelP <- (ifelse(test$health=="Poor",1,0))
gbm.probsT <- as.data.frame(gbm.probsT)

# Evaluation for Good vs. Fair/Poor
gbm_scoresGT <- prediction(predictions=gbm.probsT[,2], labels=test$labelG)
# ROC Curve for Good vs. Fair/Poor
gbm_perfGT <- performance(gbm_scoresGT, "tpr", "fpr") # ROC Curve for GBM Model Good vs. Fair/Poor
plot(gbm_perfGT,
     main="ROC Curve for GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Good vs. Fair/Poor
gbm_aucGT <- performance(gbm_scoresGT, "auc")
as.numeric(gbm_aucGT@y.values)
# Lift Chart for Good vs. Fair/Poor
gbm_liftGT <- performance(gbm_scoresGT, measure="lift", x.measure="rpp")
plot(gbm_liftGT,
     main="Lift Chart for GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation for Fair vs. Good/Poor
gbm_scoresFT <- prediction(predictions=gbm.probsT[,1], labels=test$labelF)
# ROC Curve for Fair vs. Good/Poor
gbm_perfFT <- performance(gbm_scoresFT, "tpr", "fpr") # ROC Curve for GBM Model Fair vs. Good/Poor
plot(gbm_perfFT,
     main="ROC Curve for GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Fair vs. Good/Poor
gbm_aucFT <- performance(gbm_scoresFT, "auc")
as.numeric(gbm_aucFT@y.values)
# Lift Chart for Fair vs. Good/Poor
gbm_liftFT <- performance(gbm_scoresFT, measure="lift", x.measure="rpp")
plot(gbm_liftFT,
     main="Lift Chart for GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation for Poor vs. Good/Fair
gbm_scoresPT <- prediction(predictions=gbm.probsT[,3], labels=test$labelP)
# ROC Curve for Poor vs. Good/Fair
gbm_perfPT <- performance(gbm_scoresPT, "tpr", "fpr") # ROC Curve for GBM Model Poor vs. Good/Fair
plot(gbm_perfPT,
     main="ROC Curve for GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Poor vs. Good/Fair
gbm_aucPT <- performance(gbm_scoresPT, "auc")
as.numeric(gbm_aucPT@y.values)
# Lift Chart for Poor vs. Good/Fair
gbm_liftPT <- performance(gbm_scoresPT, measure="lift", x.measure="rpp")
plot(gbm_liftPT,
     main="Lift Chart for GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation of UpSampled Training Set Model (ROC Curve, AUC, Lift Chart) #####
test <- cbind(test,gbm.probsUT)
test$labelG <- (ifelse(test$health=="Good",1,0))
test$labelF <- (ifelse(test$health=="Fair",1,0))
test$labelP <- (ifelse(test$health=="Poor",1,0))
gbm.probsUT <- as.data.frame(gbm.probsUT)

# Evaluation for Good vs. Fair/Poor
gbm_scoresGUT <- prediction(predictions=gbm.probsUT[,2], labels=test$labelG)
# ROC Curve for Good vs. Fair/Poor
gbm_perfGUT <- performance(gbm_scoresGUT, "tpr", "fpr") # ROC Curve for Good vs. Fair/Poor
plot(gbm_perfGUT,
     main="ROC Curve for Upsampled GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Good vs. Fair/Poor
gbm_aucGUT <- performance(gbm_scoresGUT, "auc")
as.numeric(gbm_aucGUT@y.values)
# Lift Chart for Good vs. Fair/Poor
gbm_liftGUT <- performance(gbm_scoresGUT, measure="lift", x.measure="rpp")
plot(gbm_liftGUT,
     main="Lift Chart for Upsampled GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation for Fair vs. Good/Poor
gbm_scoresFUT <- prediction(predictions=gbm.probsUT[,1], labels=test$labelF)
# ROC Curve for Fair vs. Good/Poor
gbm_perfFUT <- performance(gbm_scoresFUT, "tpr", "fpr") # ROC Curve for Fair vs. Good/Poor
plot(gbm_perfFUT,
     main="ROC Curve for Upsampled GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Fair vs. Good/Poor
gbm_aucFUT <- performance(gbm_scoresFUT, "auc")
as.numeric(gbm_aucFUT@y.values)
# Lift Chart for Fair vs. Good/Poor
gbm_liftFUT <- performance(gbm_scoresFUT, measure="lift", x.measure="rpp")
plot(gbm_liftFUT,
     main="Lift Chart for Upsampled GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation for Poor vs. Good/Fair
gbm_scoresPUT <- prediction(predictions=gbm.probsUT[,3], labels=test$labelP)
# ROC Curve for Poor vs. Good/Fair
gbm_perfPUT <- performance(gbm_scoresPUT, "tpr", "fpr") # ROC Curve for Poor vs. Good/Fair
plot(gbm_perfPUT,
     main="ROC Curve for Upsampled GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Poor vs. Good/Fair
gbm_aucPUT <- performance(gbm_scoresPUT, "auc")
as.numeric(gbm_aucPUT@y.values)
# Lift Chart for Poor vs. Good/Fair
gbm_liftPUT <- performance(gbm_scoresPUT, measure="lift", x.measure="rpp")
plot(gbm_liftPUT,
     main="Lift Chart for Upsampled GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")


# Evaluation of DownSampled Training Set Model (ROC Curve, AUC, Lift Chart) #####
test <- cbind(test,gbm.probsDT)
test$labelG <- (ifelse(test$health=="Good",1,0))
gbm.probsDT <- as.data.frame(gbm.probsDT)
# Evaluation for Good vs. Fair/Poor
gbm_scoresGDT <- prediction(predictions=gbm.probsDT[,2], labels=test$labelG)
# ROC Curve for Good vs. Fair/Poor
gbm_perfGDT <- performance(gbm_scoresGDT, "tpr", "fpr") # ROC Curve for Good vs. Fair/Poor
plot(gbm_perfGDT,
     main="ROC Curve for Downsampled GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Good vs. Fair/Poor
gbm_aucGDT <- performance(gbm_scoresGDT, "auc")
as.numeric(gbm_aucGDT@y.values)
# Lift Chart for Good vs. Fair/Poor
gbm_liftGDT <- performance(gbm_scoresGDT, measure="lift", x.measure="rpp")
plot(gbm_liftGDT,
     main="Lift Chart for Downsampled GBM Model: One (Good) vs. All (Fair/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")

# Evaluation for Fair vs. Good/Poor
gbm_scoresFDT <- prediction(predictions=gbm.probsDT[,1], labels=test$labelF)
# ROC Curve for Fair vs. Good/Poor
gbm_perfFDT <- performance(gbm_scoresFDT, "tpr", "fpr") # ROC Curve for Fair vs. Good/Poor
plot(gbm_perfFDT,
     main="ROC Curve for Downsampled GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Fair vs. Good/Poor
gbm_aucFDT <- performance(gbm_scoresFDT, "auc")
as.numeric(gbm_aucFDT@y.values)
# Lift Chart for Fair vs. Good/Poor
gbm_liftFDT <- performance(gbm_scoresFDT, measure="lift", x.measure="rpp")
plot(gbm_liftFDT,
     main="Lift Chart for Downsampled GBM Model: One (Fair) vs. All (Good/Poor)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")


# Evaluation for Poor vs. Good/Fair
gbm_scoresPDT <- prediction(predictions=gbm.probsDT[,3], labels=test$labelP)
# ROC Curve for Poor vs. Good/Fair
gbm_perfPDT <- performance(gbm_scoresPDT, "tpr", "fpr") # ROC Curve for Poor vs. Good/Fair
plot(gbm_perfPDT,
     main="ROC Curve for Downsampled GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="darkblue",  lwd = 3)
abline(0,1, lty = 300, col = "green",  lwd = 3)
grid(col="aquamarine")
# Area Under The Curve for Poor vs. Good/Fair
gbm_aucPDT <- performance(gbm_scoresPDT, "auc")
as.numeric(gbm_aucPDT@y.values)
# Lift Chart for Poor vs. Good/Fair
gbm_liftPDT <- performance(gbm_scoresPDT, measure="lift", x.measure="rpp")
plot(gbm_liftPDT,
     main="Lift Chart for Downsampled GBM Model: One (Poor) vs. All (Good/Fair)",
     xlab="% Populations (Percentile)",
     ylab="Lift",
     col="darkblue", lwd = 3)
abline(1,0,col="red",  lwd = 3)
grid(col="aquamarine")
