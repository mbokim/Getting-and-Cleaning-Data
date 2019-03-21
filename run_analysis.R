#data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#Loading package
library(dplyr)

#1.Merge the training and the test sets to create one data set.

#Download dataset
downloadurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  filename <- "UCI HAR Dataset.zip"
  download.file(downloadurl, filename, method="curl")

# unzip the folder
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
  }
#data frames
  
  features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code") 

  all_x <- rbind(x_train, x_test)
  all_y <- rbind(y_train, y_test)
  all_subject <- rbind(subject_train, subject_test)
  one_data <- cbind(all_subject, all_y, all_x)  
  
#2. Extract only the measurements on the mean and standard deviation for each measurement
  final_data <- one_data %>% select(subject, code, contains("mean"), contains("std"))
  
#3. Use descriptive activity names to name the activities in the data set
  final_data$code <- activities[final_data$code, 2]
  
#4. Appropriately label the data set with descriptive variable names.
  names(final_data)[2] = "activity"
  names(final_data)<-gsub("Acc", "Accelerometer", names(final_data))
  names(final_data)<-gsub("Gyro", "Gyroscope", names(final_data))
  names(final_data)<-gsub("BodyBody", "Body", names(final_data))
  names(final_data)<-gsub("Mag", "Magnitude", names(final_data))
  names(final_data)<-gsub("^t", "Time", names(final_data))
  names(final_data)<-gsub("^f", "Frequency", names(final_data))
  names(final_data)<-gsub("tBody", "TimeBody", names(final_data))
  names(final_data)<-gsub("-mean()", "Mean", names(final_data), ignore.case = TRUE)
  names(final_data)<-gsub("-std()", "StandardDev", names(final_data), ignore.case = TRUE)
  names(final_data)<-gsub("-freq()", "Frequency", names(final_data), ignore.case = TRUE)
  names(final_data)<-gsub("angle", "Angle", names(final_data))
  names(final_data)<-gsub("gravity", "Gravity", names(final_data))
  
#5.  Create a second, independent tidy data set with the average of each variable for each activity and each subject.
  second_data <- final_data %>%
    group_by(subject, activity) %>%
    summarise_all(list(~mean))
  write.table(second_data, "SecondData.txt", row.name=FALSE)
  
