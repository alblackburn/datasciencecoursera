# run_analysis.R
# This script processes data collected from
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# The script reads in and correlates all phone data, plus activity names and subject IDs.
# It renames variables into an acceptable tidy data format.
# The script collects only those variables concerned with the mean or standard deviation
# and then summarizes them by subject ID and then by activity.
# 
# Allen Blackburn
# 1/31/2016

# Load libraries
library(dplyr)

# Read in raw test and training data
testRaw <- read.table("data/test/X_test.txt")
trainRaw <- read.table("data/train/X_train.txt")

# Read in activity name correlations
testActivity <- read.table("data/test/y_test.txt")
trainActivity <- read.table("data/train/y_train.txt")

# Read in test and training subject IDs
testSubject <- read.table("data/test/subject_test.txt")
trainSubject <- read.table("data/train/subject_train.txt")

# Read in variable correlations
features <- read.table("data/features.txt")

# Bind test and training data
testTrainRaw <- rbind(testRaw, trainRaw)

# Add variable names to combined data as column names
# Also ensure variables names are unique
variables <- features[,2]
colnames(testTrainRaw) <- variables
colnames(testTrainRaw) <- names(testTrainRaw[,])

# Gather "mean" and "std" variables
meanStdVars <- grep("mean|std", colnames(testTrainRaw), value = TRUE, ignore.case = TRUE)

# Select only those columns with "mean" or "std"
testTrainSelectData <- select(testTrainRaw, one_of(meanStdVars))

# Bind activity test and training activity names and add column name
testTrainActivity <- rbind(testActivity, trainActivity)
colnames(testTrainActivity) <- c("activityName")

# Bind subject test and training subject IDs and add column name
testTrainSubject <- rbind(testSubject, trainSubject)
colnames(testTrainSubject) <- c("subjectID")

# Bind activity names and subject ID to data
testTrainData <- cbind(testTrainSubject, testTrainActivity, testTrainSelectData)

# Read in activity name correlations
activities <- read.table("data/activity_labels.txt")

# Substitute activity numbers with activity names
for (i in seq(1:nrow(activities))) {
        testTrainData$activityName <- gsub(activities$V1[i], activities$V2[i], testTrainData$activityName)    
}

# Pull out variable names, rename them so they are easier to understand,
# and overwrite variable names. Also remove any non-alphabetic characters.
colnames(testTrainData) <- gsub("Acc", "Accelerometer", colnames(testTrainData))
colnames(testTrainData) <- gsub("Gyro", "Gyroscope", colnames(testTrainData))
colnames(testTrainData) <- gsub("Mag", "Magnitude", colnames(testTrainData))
colnames(testTrainData) <- gsub("Freq", "Frequency", colnames(testTrainData))
colnames(testTrainData) <- gsub("mean", "Mean", colnames(testTrainData))
colnames(testTrainData) <- gsub("std", "Std", colnames(testTrainData))
colnames(testTrainData) <- gsub("gravity", "Gravity", colnames(testTrainData))
colnames(testTrainData) <- gsub("anglet", "angleTime", colnames(testTrainData))
colnames(testTrainData) <- gsub("^t", "time", colnames(testTrainData))
colnames(testTrainData) <- gsub("^f", "frequency", colnames(testTrainData))
colnames(testTrainData) <- gsub("\\(", "", colnames(testTrainData))
colnames(testTrainData) <- gsub("\\)", "", colnames(testTrainData))
colnames(testTrainData) <- gsub("-", "", colnames(testTrainData))
colnames(testTrainData) <- gsub(",", "", colnames(testTrainData))

# Group data set by subject ID, then by Activity
testTrainDataGrouped <- group_by(testTrainData, subjectID, activityName)

# Summarize on multiple columns using summarize_each
# Do no summarize on subjectID or activityName columns
# This is our tidy data set!
tidyDataSet <- summarize_each_(testTrainDataGrouped, "mean", names(testTrainData)[-(1:2)])

print(tidyDataSet)

write.table(tidyDataSet, file = "tidyDataSet.txt", row.name=FALSE)

