"
Getting and Cleaning Data
Course Project

Student: Jose Martin Alvarez Milan
 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 

1) a tidy data set as described below, 
2) a link to a Github repository with your script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.
Requirements:

A) Merges the training and the test sets to create one data set.
B) Extracts only the measurements on the mean and standard deviation for each measurement.
C) Uses descriptive activity names to name the activities in the data set
D) Appropriately labels the data set with descriptive variable names.
E) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
"

rm(list = ls())
library(dplyr)

"Preparation:  Load data into R data sets"


"Train data sets"
trainX <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/train/x_train.txt")
trainY <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/train/y_train.txt")
subjectTrain <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/train/subject_train.txt")

"Test data sets"
testX <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/test/x_test.txt")
testY <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/test/y_test.txt")
subjectTest <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/test/subject_test.txt")

"
1st Requirement: Merge the training and test data sets into one data set.
"

dataX<- rbind(trainX,testX)
labelY <- rbind(trainY,testY)
subject <- rbind(subjectTrain,subjectTest)

"
2nd Requirement: Extract the mean and standard deviation for each measurement.
"
features <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/features.txt")
dataFeatures <- grep("mean\\(\\)|std\\(\\)", features[,2])
dataPoints <- dataX[,dataFeatures]
variables <- features[dataFeatures,]

"
3rd Requirement: Provide descriptive activity names to name the activities in the data set
4th Requirement: Appropriately labels the data set with descriptive variable names.
NOTE from features info:   
(prefix 't' to denote time)
(Note the 'f' to indicate frequency domain signals)
"
names(variables) <- c("id", "columnName")
variables <- variables %>%
  mutate(columnName = gsub("-mean\\(\\)","_mean", tolower(columnName)),
         columnName = gsub("-std\\(\\)","_std", tolower(columnName)),
         columnName = gsub("-","_", tolower(columnName)),
         columnName = gsub("^t","time_", tolower(columnName)),
         columnName = gsub("^f","frequency_", tolower(columnName)),
         columnName = gsub("BodyBody","body", tolower(columnName)))

names(dataPoints) <- variables[,2]
activityLabels <- read.table("D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/activity_labels.txt")
names(activityLabels) <- c("id", "activity")
names(labelY)[1] <- "id"
names(subject)[1] <- "subjectId"
activity <- labelY %>%
 left_join(activityLabels, by = "id") %>%
 select(activity)
allData <- cbind(subject,activity,dataPoints)

"
5th Requirement: FUsing the data set from requirement 4), generate a tidy data set with the average of each variable for each activity and each subject.
Note:  File format of the tidy result set is csv but the file is identified as txt 
"

tidyData <- allData %>%
  group_by(subjectId, activity) %>%
  summarise_each(funs(mean))
write.table(tidyData, "D:/projects/Classes/Dscience classes/Class3 Prep Data/getting and cleaning data project/tidyDataResult.txt", row.name = FALSE, sep=",")
