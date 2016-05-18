
library(data.table)
library(dplyr)

# Download DATA
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, destfile = "dataset.zip", mode = "wb")

# unzip and delete the dataset.zip
unzip("dataset.zip")
unlink("dataset.zip")

# Read DATA
features <- read.table("UCI HAR Dataset/features.txt")
features <- as.character(features[,2])

train_X <- read.table("UCI HAR Dataset/train/X_train.txt")
train_activity <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

train <- data.frame(train_subject, train_activity, train_X)
names(train) <- c(c('subject', 'activity'), features)

test_X <- read.table("UCI HAR Dataset/test/X_test.txt")
test_activity <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

test <-  data.frame(test_subject, test_activity, test_X)
names(test) <- c(c('subject', 'activity'), features)

######################################################################################################
# 1 - Merges the training and the test sets to create one data set.
######################################################################################################

# Rowbing
OneDataSet <- rbind(train, test)


######################################################################################################
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
######################################################################################################

col_selection <- grep('mean|std', features)
extract <- OneDataSet[,c(1,2,col_selection + 2)]


######################################################################################################
# 3 - Uses descriptive activity names to name the activities in the data set
######################################################################################################

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
activity_labels <- as.character(activity_labels[,2])

extract$activity <- activity_labels[extract$activity]

######################################################################################################
# 4 - Appropriately labels the data set with descriptive variable names.
######################################################################################################

name <- names(extract)
name <- gsub("[(][)]", "", name)
name <- gsub("^t", "Time_", name)
name <- gsub("^f", "Frequency_", name)
name <- gsub("Acc", "Accelerometer", name)
name <- gsub("Gyro", "Gyroscope", name)
name <- gsub("Mag", "Magnitude", name)
name <- gsub("-mean-", "_Mean_", name)
name <- gsub("-std-", "_StandardDeviation_", name)
name <- gsub("-", "_", name)
names(extract) <- name


######################################################################################################
# 5 - From the data set in step 4, creates a second, independent tidy data set with the average of 
#     each variable for each activity and each subject.
######################################################################################################

tidy <- aggregate(extract[,3:81], 
                  by = list(activity = extract$activity, subject = extract$subject)
                  ,FUN = mean)

write.table(x = tidy, file = "data_tidy.txt", row.names = FALSE)























