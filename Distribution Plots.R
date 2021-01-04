library(pastecs)#used for describing data sets
library(ggplot2)

rawPath = "C:/Users/evan.lynch/Documents/R/Projects/Class Project/raw_original_021919.csv"
data_raw = read.csv(file=rawPath,header=TRUE,sep=",")
View(data_raw)


PDFPath = "C:\\Users\\evan.lynch\\Documents\\R\\Projects\\Class Project\\DistributionPlots.pdf"
pdf(file = PDFPath)

#create a distribution plot for variables of interest
g = ggplot(data_raw,aes(health))
g + geom_bar()

g = ggplot(data_raw,aes(curb_loc,fill=health))
g + geom_bar()

g = ggplot(data_raw,aes(status,fill=health))
g + geom_bar()

#hard to see but it looks like null values are all one tree type
g = ggplot(data_raw,aes(spc_latin,fill=health))
g + geom_bar()

print("number of unique types of trees (latin)")
length(unique(data_raw$spc_latin))

print("number of unique types of trees (common)")
length(unique(data_raw$spc_common))

g = ggplot(data_raw,aes(steward,fill=health))
g + geom_bar()

g = ggplot(data_raw,aes(guards,fill=health))
g + geom_bar()

g = ggplot(data_raw,aes(sidewalk,fill=health))
g + geom_bar()


comment = "this looks like good news. Wouldn't want this to be a big predictor"
g = ggplot(data_raw,aes(user_type,fill=health)) + ggtitle(comment)
g + geom_bar()


unique(data_raw$problems)

comment = "can we even transform this"
g = ggplot(data_raw,aes(root_grate,fill=health)) + ggtitle(comment)
g + geom_bar()

g = ggplot(data_raw,aes(trunk_wire,fill=health))
g + geom_bar()

#can we even transform this?
g = ggplot(data_raw,aes(trnk_light,fill=health)) + ggtitle(comment)
g + geom_bar()

#can we even transform this?
g = ggplot(data_raw,aes(trnk_other,fill=health)) + ggtitle(comment)
g + geom_bar()

g = ggplot(data_raw,aes(brch_light,fill=health))
g + geom_bar()

#can we even transform this?
g = ggplot(data_raw,aes(brch_shoe,fill=health)) + ggtitle(comment)
g + geom_bar()

#can we even transform this?
g = ggplot(data_raw,aes(brch_other,fill=health)) + ggtitle(comment)
g + geom_bar()

g = ggplot(data_raw,aes(zip_city,fill=health))
g + geom_bar()

comment = "good distribution here"
g = ggplot(data_raw,aes(borough,fill=health)) + ggtitle(comment)
g + geom_bar()

dev.off()

names(data_raw)
