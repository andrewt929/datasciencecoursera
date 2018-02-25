# About:
##To operate, execute the "run_anlaysis.R" file in R.
This file pulls data from: "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip".

A description of this data can be found here: "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones".

**run_analysis.R** will do 5 things:
1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The output of the file is a "TidyData.txt" file with the tidy data. 

The file uses the **data.table**, **dplyr**, and **tidyr** packages. 

Further detail of the code can be found in the "CodeBook.md" file included in this directory.