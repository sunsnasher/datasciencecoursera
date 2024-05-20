#. Notation explained:
#. # -> Denote an operation or action.
#. #. -> Denote a side note.
#. #// -> Denote codes that can be executed.

# Load the tidyverse package for easier data analysis than using Base R.
library(tidyverse)

# Read the two data sets: (1) training set (2) testing set
trainingset <- read_table("./UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
testset <- read_table("./UCI HAR Dataset/test/X_test.txt", col_names = FALSE)
#. The read_table function in the readr package does the same thing as 
#. the read.table function in Base R. The following produce the same results:
#// trainingset <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
#// testset <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")


#1. Merges the training and the test sets to create one data set

maindata <- rbind(trainingset, testset)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Read the features file, which contains the name of all 561 columns.
features <- read_table("./UCI HAR Dataset/features.txt", col_names = FALSE)

# Name all the 561 columns in mergeddata
column_names <- pull(features,2)
#. Note: under dplyr, it seems that features[2] will not produce a vector,
#. instead, it still remains a data frame (tibble). Use [[2]] or pull() instead.
column_names <- make.unique(column_names)
#. There are duplicated values. Column names should ideally be unique.
colnames(maindata) <- column_names

# Subset columns that have mean() and std() in column names.
index <- grep("mean\\(\\)|std\\(\\)", column_names)
#. If we do not include "\\(\\)" in the regex, it will find "meanFreq()".
maindata2 <- maindata[,index]


#3. Uses descriptive activity names to name the activities in the data set

# Read the activity labels files for training / test
traininglabels <- read_table("./UCI HAR Dataset/train/y_train.txt", col_names = FALSE)
testlabels <- read_table("./UCI HAR Dataset/test/y_test.txt", col_names = FALSE)

# "Attach" activity labels to the training set data and test set data
labels <- rbind(traininglabels, testlabels)
maindata3 <- cbind(labels, maindata2) 
maindata3 <- maindata3 %>% rename(activity.labels = X1)

# Create a data frame called "activitylabels" for matching labels with activities
c1 <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", 
        "STANDING", "LAYING")
activitylabels <- data.frame("activity.labels" = 1:6, "activity.names" = c1)

# Join "activitylabels" with "mergeddata2", effectively adding a "activity.names"
# column that matches labels
maindata3 <- left_join(maindata3, activitylabels, by="activity.labels")
#. left_join is a function within the tidyverse/dplyr package

# Move the "activity.names" column to the first column
maindata3 <- maindata3[,c(68,1:67)]


#4. Appropriately labels the data set with descriptive variable names

descriptive_names <- function(name) {
    name <- gsub("^t", "time", name)
    name <- gsub("^f", "frequency", name)
    name <- gsub("Acc", "Accelerometer", name)
    name <- gsub("Gyro", "Gyroscope", name)
    name <- gsub("Mag", "Magnitude", name)
    name <- gsub("\\(\\)", "", name)
    return(name)
}
#. Use "^t" & "^f", instead of "t" & "f". Otherwise, it will be a mess.

column_names2 <- descriptive_names(colnames(maindata3))
maindata4 <- maindata3
colnames(maindata4) <- column_names2


#5.From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.

# Read subject data (p.s. "subjects" = participants)
subject_train <- read_table("./UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
subject_test <- read_table("./UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)

# Create a data frame for the data of subject 
subject <- rbind(subject_train, subject_test)
maindata5 <- cbind(subject, maindata4)

# Change the name of the first column to "subject"
maindata5 <- maindata5 %>% rename(subject = X1)

# For each subject and each activity, calculate mean

averagedata <- maindata5 %>% 
    group_by(subject, activity.names) %>%
    summarise_all(.fun = c(mean="mean"))

#. By denoting mean="mean", each column name will be added a suffix "_mean".
#. The 3rd column "activity.label" will also be calculated means but that 
#. does not matter.
#. The following produces the same but excludes the 3rd column "activity.label":
#// summarise(across(!activity.labels, mean, .names = "{.col}_mean"))
#. This also works (without suffix):
#// summarise(across(everything(), mean))

# Change the name of column activity.labels to prevent unnecessary confusion
averagedata <- averagedata %>% rename(activity.labels = activity.labels_mean)

# Write averagedata into a txt file
write.table(averagedata, "./UCI HAR Dataset/averagedata.txt", row.name=TRUE)













