# Require | Install Packages
    if (!require("data.table")) {
    install.packages("data.table")
    }
    if (!require("dplyr")) {
    install.packages("dplyr")
    }
    if (!require("tidyr")) {
    install.packages("data.table")
    }


    require("data.table")
    require("dplyr")
    require("tidyr")

# Download & Unzip the data
    url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    downloadPath <- file.path(getwd(), "UCI HAR Dataset.zip")
    download.file(url, downloadPath)
    unzip(filePath)
    filePath <- file.path(getwd(), "UCI HAR Dataset")
    
    
# TASK 1: Merge the Training & Test sets to create one data set
## Load Training data.
    subject_train <- tbl_df(read.table(file.path(filePath, "train", "subject_train.txt")))
    activity_train <- tbl_df(read.table(file.path(filePath, "train", "y_train.txt")))
    data_train <- tbl_df(read.table(file.path(filePath, "train", "X_train.txt" )))

## Load Testing data.
    subject_test  <- tbl_df(read.table(file.path(filePath, "test" , "subject_test.txt" )))
    activity_test  <- tbl_df(read.table(file.path(filePath, "test" , "y_test.txt" )))
    data_test <- tbl_df(read.table(file.path(filePath, "test" , "X_test.txt" )))

## Load general information
    features <- tbl_df(read.table(file.path(filePath, "features.txt")))
      setnames(features, names(features), c("featureIndex", "featureName"))
    activityLabels<- tbl_df(read.table(file.path(filePath, "activity_labels.txt")))
      setnames(activityLabels, names(activityLabels), c("activityIndex","activityName"))

## Combine tables
    all_subject <- rbind(subject_train, subject_test)
      setnames(all_subject, "V1", "subject")
    all_activity<- rbind(activity_train, activity_test)
      setnames(all_activity, "V1", "activityIndex")
    all_data <- rbind(data_train, data_test)
      colnames(all_data) <- features$featureName

## Merge names with indexes      
    all_SubAct <- cbind(all_subject, all_activity)
    all_data <- cbind(all_SubAct, all_data)
 
         
# TASK 2: Extract only the measurements on the mean and standard deviation for each measurement.
    featuresMeanStd <- grep("(mean|std)\\(\\)", features$featureName, value=TRUE)
    featuresMeanStd <- union(c("subject","activityIndex"), featuresMeanStd)
    all_data <- subset(all_data, select = featuresMeanStd)
   
    
# TASK 3: Uses descriptive activity names to name the activities in the data set
## Merge names with indexes
    all_data <- merge(activityLabels, all_data, by = "activityIndex", all.x = TRUE)
    all_data$activityName <- as.character(all_data$activityName)
  
      
# TASK 4: Appropriately label the data set with descriptive variable names.
    names(all_data) <- gsub("std()", "SD", names(all_data))
    names(all_data) <- gsub("mean()", "MEAN", names(all_data))
    names(all_data) <- gsub("^t", "time", names(all_data))
    names(all_data) <- gsub("^f", "frequency", names(all_data))
    names(all_data) <- gsub("Acc", "Accelerometer", names(all_data))
    names(all_data) <- gsub("Mag", "Magnitude", names(all_data))
    names(all_data) <- gsub("BodyBody", "Body", names(all_data))


# TASK 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Create tidy data
    all_data$activityName <- as.character(all_data$activityName)
    agg_data <- aggregate(.~subject - activityName, data = all_data, mean)
    all_data <- tbl_df(arrange(agg_data, subject, activityName))
## Write tidy data to new file
    write.table(all_data, "TidyData.txt", row.names = FALSE)