library(dplyr)
library(reshape2)

# Checking if archieve already exists.
if (!file.exists("./Assigmentweek4.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                  "./Assigmentweek4.zip", method="curl")
} 

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
    unzip("Assigmentweek4.zip") 
}

# STEP 1: Merges the training and the test sets to create one data set

# Extract train and test data set
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", quote="\"", comment.char="")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt", quote="\"", comment.char="")

# Extract train and test subject
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")

# Extract features and activities
features_name <- read.table("./UCI HAR Dataset/features.txt", quote="\"", comment.char="")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")

# adding column names
names(subject_train) <- "ID_subject"
names(subject_test) <- "ID_subject"

names(X_train) <- features_name$V2
names(X_test) <- features_name$V2

names(Y_train) <- "activity"
names(Y_test) <- "activity"

# Merging data
X_data <- rbind(X_train,X_test)
Y_data <- rbind(Y_train,Y_test)
subject <- rbind(subject_train, subject_test)
Dataset <- cbind(subject,Y_data,X_data)


# STEP 2: Extracting mean and std measurements
features_name$V2 <- as.character(features_name$V2)
subset_name <- features_name %>% group_by(V2) %>% mutate(n = dense_rank(V1)) %>% filter(n == 1) %>% ungroup()
subset_name <- subset_name[grepl("mean\\(\\)|std\\(\\)", subset_name$V2),]
Dataset <- Dataset[,c("ID_subject","activity",subset_name$V2)]

# STEP 3: Uses descriptive activity names to name the activities in the data set.
Dataset <- Dataset %>% left_join(activities, by = c("activity" = "V1")) %>% 
    select(-activity)
Dataset <- Dataset %>% rename(activity = V2)


# STEP 4: Appropriately labels the data set with descriptive variable names.
names(Dataset)<-gsub("std()", "stdev", names(Dataset))
names(Dataset)<-gsub("mean()", "mean", names(Dataset))
names(Dataset)<-gsub("^t", "time", names(Dataset))
names(Dataset)<-gsub("^f", "frequency", names(Dataset))
names(Dataset)<-gsub("Acc", "Accelerometer", names(Dataset))
names(Dataset)<-gsub("Gyro", "Gyroscope", names(Dataset))
names(Dataset)<-gsub("Mag", "Magnitude", names(Dataset))
names(Dataset)<-gsub("BodyBody", "Body", names(Dataset))

# STEP 5: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_Dataset <- Dataset %>% group_by(ID_subject, activity) %>% summarise_all(mean)

write.table(tidy_Dataset, "tidy Dataset.txt", row.name=FALSE)
