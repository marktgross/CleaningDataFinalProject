run_analysis <- function() {
  
  ##read in six Samsung files to be consolidated for test and training subjects
  ##per submission instructions, function is designed to run with assumption that all Samsung files are in working directory
    subject_test <- read.table("subject_test.txt")
    activity_test <- read.table("y_test.txt")
    variables_test <- read.table("X_test.txt")
    subject_train <- read.table("subject_train.txt")
    activity_train <- read.table("y_train.txt")
    variables_train <- read.table("X_train.txt")
  
  ##Step 1: Merge the training and the test sets to create one data set
    ##merge test files together using cbind function
    test <- cbind(subject_test, activity_test, variables_test)
    ##merge training files together using cbind function
    train <- cbind(subject_train, activity_train, variables_train)
  
    ##merge consolidated training and test sets together using rbind
    tt <- rbind(train, test)
    
  ##Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
    ##import data from features.txt
    features <- read.table("features.txt")
    ##find positions of values in features that include "mean()" or "std()"
      ##this only includes values with "mean()" and "std()"; does not included Frequency Mean or Angular Mean values
    pos <- grep("mean\\(|std\\(",features$V2)
    ##add 2 to positions to account for two additional columns in consolidated table (tt)
      ##include columns 1 & 2 in positions vector to account for subjects & activities
    pos <- c(1, 2, (pos + 2))
    ##create subset to only include variables with "mean()" or "std()"
    tt_sub <- tt[,pos]
    
  ##Step 3: Uses descriptive activity names to name the activities in the data set
    ##replace numeric activity labels with descriptive entries for activity names
    tt_sub[,2][tt_sub[,2] == 1] <- "Walking"
    tt_sub[,2][tt_sub[,2] == 2] <- "Walking Upstairs"
    tt_sub[,2][tt_sub[,2] == 3] <- "Walking Downstairs"
    tt_sub[,2][tt_sub[,2] == 4] <- "Sitting"
    tt_sub[,2][tt_sub[,2] == 5] <- "Standing"
    tt_sub[,2][tt_sub[,2] == 6] <- "Laying"
    
  ##Step 4: Appropriately labels the data set with descriptive variable names
    ##create character vector with names of first two variables (subject & activity)
    vnames1 <- c("Subject", "Activity")
    ##find all of the current variable names for the desired columns from features.txt
    vnames2 <- grep("mean\\(|std\\(",features$V2, value=TRUE)
      ##replace starting "t" with "Time_"
      vnames2 <- gsub("^t","Time_",vnames2)
      ##replace starting "f" with "Frequency_"
      vnames2 <- gsub("^f","Frequency_",vnames2)
      ##replace "Acc" with "Accelerometer_"
      vnames2 <- gsub("Acc", "Accelerometer_",vnames2)
      ##replace "Gyro" with "Gyroscope_"
      vnames2 <- gsub("Gyro", "Gyroscope_",vnames2)
      ##replace "Jerk" with "JerkSignal_" 
      vnames2 <- gsub("Jerk", "JerkSignal_",vnames2)
      ##replace "Mag" with "Magnitude_"
      vnames2 <- gsub("Mag", "Magnitude_",vnames2)
      ##account for records with repeated "BodyBody"
      vnames2 <- gsub("BodyBody", "Body",vnames2)
      ##replace "-mean()-" with "Mean_"
      vnames2 <- gsub("\\-mean\\(\\)\\-", "Mean_",vnames2)
      ##replace "-mean()" with "Mean"
      vnames2 <- gsub("\\-mean\\(\\)", "Mean",vnames2) 
      ##replace "-std()-" with "StandDev_"
      vnames2 <- gsub("\\-std\\(\\)\\-", "StandDev_",vnames2)
      ##replace "-std()" with "StandDev"
      vnames2 <- gsub("\\-std\\(\\)", "StandDev",vnames2)
      
    ##combine both column name vectors
    vnames <- c(vnames1,vnames2)
    ##change variable names in tt_sub  
    names(tt_sub) <- vnames
    
  ##Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
    ##for each activity and each subject.
    ##rearrange current table to order by subject & activity
    final <- arrange(tt_sub, Subject, Activity) %>%
      ##group data by subject, activity
      group_by(Subject, Activity) %>%
      ##summarize the data to calculate the mean for each variable; this calculates the mean of each variable 
        ##for each activty for each subject
      summarize_each(funs(mean))
    
    ##return final table - this table is tidy in the wide form
    final
    
}