# This script will do the following things:
#   1. Merges the training and the test sets to create one data set.
#   2. Extracts only the measurements on the mean and standard deviation for each measurement.
#   3. Uses descriptive activity names to name the activities in the data set
#   4. Appropriately labels the data set with descriptive variable names.
#   5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# loading packages:
library(data.table)


# downloading the dataset file in the current directory of this R script:
data_file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(data_file_url, destfile = "dataset.zip", method = "curl")

# unzip the downloaded file in the current directory of this R script:
unzip("dataset.zip")


# renaming and loading the activity_labels.txt file
activity_labels <- fread("UCI HAR Dataset/activity_labels.txt", col.names = c("classLabels", "activityName"))
# renaming and loading the features.txt file
features <- fread("UCI HAR Dataset/features.txt", col.names = c("index", "featureNames"))
# extracting the mean and std feature names from the features.txt file
features_with_mean_std <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[features_with_mean_std, featureNames]
# removing the parenthesis from the feature names
measurements <- gsub('[()]', '', measurements)


# loading the X_train.txt, y_train.txt and subject_train.txt datasets:
train_dataset <- fread("UCI HAR Dataset/train/X_train.txt")[, features_with_mean_std, with = FALSE]
# renaming train_dataset's column names to the feature names
setnames(train_dataset, colnames(train_dataset), measurements)
train_activities <- fread("UCI HAR Dataset/train/y_train.txt", col.names = "activity")
train_subjects <- fread("UCI HAR Dataset/train/subject_train.txt", col.names = "subjectNumber")
# adding the subjects and activities column to the train_dataset
train_dataset <- cbind(train_subjects, train_activities, train_dataset)


# loading the X_test.txt, Y_test.txt and the subject_test.txt dataset
test_dataset <- fread("UCI HAR Dataset/test/X_test.txt")[, features_with_mean_std, with = FALSE]
# renaming the X_test dataset columns to measurements
setnames(test_dataset, colnames(test_dataset), measurements)
test_activities <- fread("UCI HAR Dataset/test/Y_test.txt", col.names = "activity")
test_subjects <- fread("UCI HAR Dataset/test/subject_test.txt", col.names = "subjectNumber")
# adding the subjects and activities column to the test_dataset
test_dataset <- cbind(test_subjects, test_activities, test_dataset)


# merging the train and test datasets
merged_dataset <- rbind(train_dataset, test_dataset)

# converting classLabels to activityName and the column activity as factor type
merged_dataset[["activity"]] <- factor(merged_dataset[, activity], levels = activity_labels[["classLabels"]], labels = activity_labels[["activityName"]])
# converting the subjectNumber column into a factor type
merged_dataset[["subjectNumber"]] <- as.factor(merged_dataset[, subjectNumber])

# calculating mean by the activity and subject
merged_dataset <- data.table::melt(data = merged_dataset, id.vars = c("subjectNumber", "activity"))
merged_dataset <- data.table::dcast(data = merged_dataset, formula = subjectNumber + activity ~ variable, fun.aggregate = mean)

# writing the merged_dataset into a txt file
write.table(x = merged_dataset, file = "tidy_dataset.txt", row.names = FALSE)
