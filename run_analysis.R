library(dplyr)
library(data.table)

# read train data
featuresTrain <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/train/X_train.txt")
activityTrain <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/train/Y_train.txt") 
subjectTrain <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/train/subject_train.txt") 

#read test data
featuresTest <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/test/X_test.txt")
activityTest <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/test/Y_test.txt")
subjectTest <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/test/subject_test.txt")

# read data description
featureNames <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/features.txt")

# read activity labels
activityLabels <- read.table("C:/Users/David/Documents/R/UCI HAR Dataset/activity_labels.txt")

# assigning variables names and merges the training and test data sets into one only data set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(subject) <- "Subject"
colnames(activity) <- "Activity"
alldata <- cbind(features, activity, subject) 

# extracts only the measurements on the mean and standard deviation
onlyMeanStd <- grep(".*Mean.*|.*Std.*", names(alldata), ignore.case = TRUE)
requiredColumns <- c(onlyMeanStd, 562,563)
extractedData <- alldata[,requiredColumns]

# uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)

for (i in 1:6) {
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

# appropriately labels the data set with descriptive variable names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

# from the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity ans each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData&Subject, tidyData$Activity),]
write.table(tidyData, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE)
