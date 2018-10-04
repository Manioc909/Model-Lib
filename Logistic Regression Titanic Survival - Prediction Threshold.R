#Logistic Regression Titanic Survival - Prediction Threshold Investigation
#This will look at the logistic regression built to predict survival and establish what is the best threshold 
# to indicate survival. Initially it was set up to be 0.5 but is there a better threshold to be using for this
# dataset.

rm(list=ls())
setwd("D:/R Codes/Titanic Data")

#Import dataset and subsetting to required columns
training.data.raw <- read.csv("train.csv",header=T, na.strings=c(""))
data <- subset(training.data.raw, select = c(2,3,5,6,7,8,10,12))

#The no age records are set to have the average age. This would make another good investigation to see if there
# is a better way to set this.
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm=T)

#There are 2 rows missing Embarked and so these rows will be disgarded
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#This builds the model using Pclass, Sex and Age to predict survival.
model<-glm(Survived ~.,family=binomial(link='logit'),subset(data,select=c(1,2,3,4)))
summary(model)
anova(model,test="Chisq")




#Importing unknown outcome dataset
uo.data.raw <- read.csv("test.csv",header=T, na.strings=c(""))

#dropping columns not needed and applying average age to null ages
uo<-subset(uo.data.raw, select =c(2,4,5))
uo$Age[is.na(uo$Age)] <- mean(uo$Age, na.rm=T)

#applying the model and saving each prediction threshold
prob<-predict(model, newdata=uo, type="response")
output<-cbind(uo.data.raw,prob)

i<-1
for(i in 1:99){
  prediction.list<-as.matrix(ifelse(prob > (i/100),1,0),ncol=1)
  colnames(prediction.list)<-paste0("prediction_",i)
  output<-cbind(output,prediction.list)
}

#bringing in the final survival
final.result <- read.csv("gender_submission.csv",header=T, na.strings=c(""))
final.outcome<-merge(output, final.result)

#Histogram of the probability of survival
library(ggplot2)
ggplot(final.outcome,aes(prob))+geom_histogram(bins=100)
#Outcome of plot shows a clear cluster at ~7% and some spikes near 50%.

#Want to measure a few main points for each prediction threshold
# 1. Accuracy
# 2. Errors

