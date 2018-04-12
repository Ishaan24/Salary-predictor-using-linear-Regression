#read data from csv files
trainingData <- read.csv(file = "/Users/ishaan/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/train.csv")
testData <- read.csv(file = "/Users/ishaan/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/test.csv")

#see names of all atrributes in training data
names(trainingData)

trainingData<-na.omit(trainingData) #119 records removed for missing values of union_code

trainingData<-trainingData[!(is.na(trainingData$job_group_code) | trainingData$job_group_code==""), ] 



 #median
medianUnionCode<-median(trainingData$union_code)
testData$union_code[is.na(testData$union_code)] <- medianUnionCode

#see categories of data and their frequency to see which is present in training and test data
table(trainingData$job_group_code)
table(testData$job_group_code)

#remove Job_group_code with AIR0 and WTR as they are not present in the test data
trainingData<-subset(trainingData, job_group_code!="AIR0")
trainingData<-subset(trainingData, job_group_code!="WTR")

#using library dummies do dummy encoding(One Hot Encoding), convert categories of data to attributes and do binarization
install.packages("dummies")
library(dummies)
trainOHE<-cbind(trainingData,dummy(trainingData$job_group_code))
testOHE<-cbind(testData,dummy(testData$job_group_code))

#to confirm the columns and see how they are divided
names(trainOHE) 

#deleting columns not using
trainOHE<-trainOHE[,-8]
trainOHE<-trainOHE[,-9]
trainOHE<-trainOHE[,-9]
trainOHE<-trainOHE[,-8]
trainOHE<-trainOHE[,-7]

#using library plyr, renaming test with training in the testdata
install.packages("plyr")
library(plyr)
names(testOHE) <- gsub("test", "training", names(testOHE)) 

#scaling down, calculate mean and divide each value by the mean
meanSalary<-mean(trainOHE$salary)
trainOHE$salary <- trainOHE[, "salary"]/meanSalary

#creating the model
model<-glm(salary~.,data=trainOHE)

#predicting the salary
p<-predict(model,testOHE)

#using rio library, to export csv file
install.packages("rio")
library(rio)

df<-as.data.frame(p)
export(df, "/Users/Tirath/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/salary.csv")

#putting the salary to the submission file and exporting

salaryData <- read.csv(file = "/Users/ishaan/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/salary.csv")
names(salaryData) <- gsub("p", "salary", names(salaryData))
submission <- read.csv(file = "/Users/ishaan/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/sampleSubmission.csv")
submission[,"salary"]<-salaryData[,"salary"]
export(submission, "/Users/ishaan/Desktop/OneDrive/CMPE_256/HW/dataset-hw-1/submission.csv")


