library(diplyr)
library(caret)

# this script produces a contingency table and chi2 test comparing each categorical variable with health
# goal here is to use chi2 to test whether or not the candidate feature and health are dependent on each other
# if they are, we will continue to use them in our modeling. if not, they can drop out


data = read.csv("/Users/evan/Documents/OR568_PredictiveAnalytics/Project/dataUpSampledv2.csv") #whatever you have the original dataset called
#ignoring the index field
data = data[,-1]

colsToIgnore = c("health","tree_dbh","latitude","longitude")
for (i in colnames(data)){
  if(!(i%in%colsToIgnore)){
    # produce a contingency table 
    TAB = table(data[[i]],data$health)
   
     # run chi2 test
    CHI = chisq.test(TAB)
    
    print(i)
    print(CHI)
    print("++++++++")
  }
}
