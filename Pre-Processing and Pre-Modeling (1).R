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
data = read.csv("2015_Street_Tree_Census_-_Tree_Data.csv") #whatever you have the original dataset called
View(data)
nrow(data)
names(data)
summaryHealth <-aggregate(tree_id~health, data = data, FUN = length)
summaryHealth
summaryAlive<-aggregate(tree_id~status, data = data, FUN = length)
summaryAlive
##### Cleaning the Dataset #####
#Remove all trees in the data set that are not alive and double check it was done
data = data[data$status=="Alive", ]
summaryAlive<-aggregate(tree_id~status, data = data, FUN = length)
summaryAlive

#remove observations where health is null
data = data[!(is.na(data$health)|data$health==""), ]
write.csv(data, file = "data.csv", row.names = FALSE)
summaryHealth <-aggregate(tree_id~health, data = data, FUN = length)
data$health <- as.character(data$health)
data$health <- as.factor(data$health)
summaryHealth

# remove zv and some nzv predictors, and predictors we do not believe need to be included such as some locations 
# predictors, nta_name, guards, and others (reasoning are listed in the "Project Variables Draft.docx" document on Google Drive)
isZV <- apply(data, 2, function(x) length(unique(x)) == 1)
data <- data[, !isZV]
data <- dplyr::select(data, -"tree_id", -"block_id", -"created_at", -"address", -"postcode", -"zip_city",
                      -"community.board", -"borocode", -"borough", -"cncldist", -"st_assem", -"st_senate", -"nta",
                      -"boro_ct", -"x_sp", -"y_sp", -"council.district", -"census.tract", -"bin", 
                      -"bbl", -"problems", -"spc_latin", -"guards", -"nta_name", -"spc_common")

nearZeroVar(data, names = TRUE, saveMetrics=T) # remove predictors with nzv and frequency ratio <= 100
data <- dplyr::select(data, -"root_grate", -"trnk_light", -"brch_shoe")

##### MCA with Categorical Predictors #####
# (overall, variables do not explain variance well; 83.6% at M<p dimensions)
dataCat <- data %>% dplyr::select(-"tree_dbh", -"latitude", -"longitude")
res.mca <- MCA(dataCat, ncp=10, quali.sup=c(2), graph=FALSE) 
get_eigenvalue(res.mca) #extract eigenvalues/variances retained by each dimension
fviz_eig(res.mca) #percentage of variance explained by each dimension; 100% cumulative variance explained in 17th dimension
# below plot shows correlation of variable levels to each other, and contribution of levels to dimensions
fviz_mca_var(res.mca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, 
             ggtheme = theme_minimal())
var <- get_mca_var(res.mca) #results for variable qualitative levels
# below plot shows contribution of variable levels to first 2 dimensions
fviz_mca_var(res.mca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, ggtheme = theme_minimal()) 

fviz_contrib(res.mca, choice = "var", axes = 1:2)

# Create training and test sets for UpSampled data
set.seed(12345)
row<-nrow(data)
trainindex <- sample(row, 0.7*row, replace=FALSE)
training <- data[trainindex,]
test <- data[-trainindex,]
write.csv(test, file = "test.csv", row.names = FALSE)

##### UpSampling and DownSampling #####
# UpSample to make observations even with majority class (Good)
trainingU <- upSample(training, training$health)
trainingU <- dplyr::select(trainingU, -"Class")

# DownSample to make observations even with minority class (Poor)
trainingD <- downSample(training, training$health)
trainingD <- dplyr::select(trainingD, -"Class")

##### Writing CSVs #####
write.csv(trainingU, file = "dataUpTraining.csv", row.names = FALSE)
write.csv(trainingD, file = "dataDownTraining.csv", row.names = FALSE)
write.csv(training, file = "dataTraining.csv", row.names = FALSE)