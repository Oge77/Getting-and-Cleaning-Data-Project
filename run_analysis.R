## loading all tables
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt") ## reading the X_train Table for directory 
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt") ## reading the y_train Table for directory 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
##Merges the training and the test sets to create one data set
X_traindata <- rbind(X_train, X_test)
subject_traindata <- rbind(subject_train, subject_test)
y_traindata <- rbind(y_train, y_test)

all(is.na(traindata) == 0)

## putting the real col names
names(X_traindata) <- features[,2]
for(i in 3:563){
        X_traindata[i]<-gsub(pattern = "[V]i$",replacement = features[,i],x = names(X_traindata))
}
## Load package
library(dplyr)
subject_traindata <- rename (subject_traindata, subject = V1)
y_traindata <- rename (y_traindata, activityNum = V1)
##names(y_traindata) <- gsub(pattern = names(y_traindata),replacement = subject , x = names (y_traindata))
traindata <- cbind(subject_traindata, y_traindata, X_traindata)


##Extracts only the measurements on the mean and standard deviation for each measurement. 
##mean(): Mean value
##std(): Standard deviation

traindata_mean_std <- traindata[grepl("subject|activity|mean\\(\\)|std\\(\\)", names(traindata))]

##Uses descriptive activity names for activities
activity_labels <- rename(activity_labels, activityNum = V1, activity = V2)
traindata2 <- merge(traindata_mean_std, activity_labels, by = "activityNum")
##Exclude activity
traindata2 <- select(traindata2, -activityNum)
## resort data
traindata2 <- select(traindata2, subject, activity, 2:67)
##Appropriately labels the data set with descriptive variable names.
names(traindata2)<-gsub("-", " ", names(traindata2))
names(traindata2)<-gsub("^t", "Time ", names(traindata2))
names(traindata2)<-gsub("^f", "Frequency ", names(traindata2))
names(traindata2)<-gsub("Acc", "Accelerometer ", names(traindata2))
names(traindata2)<-gsub("Gyro", "Gyroscope ", names(traindata2))
names(traindata2)<-gsub("tBody", "Time Body ", names(traindata2))
names(traindata2)<-gsub("mean()", "Mean ", names(traindata2), ignore.case = TRUE)
names(traindata2)<-gsub("std()", "Standard deviation", names(traindata2), ignore.case = TRUE)
names(traindata2)<-gsub("angle", "Angle ", names(traindata2))
names(traindata2)<-gsub("gravity", "Gravity ", names(traindata2))
names(traindata2)<-gsub("BodyBody", "Body ", names(traindata2))
names(traindata2)<-gsub("Mag", "Magnitude ", names(traindata2))
names(traindata2)
names(traindata_mean_std)
##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidytraindata <- (traindata2%>%
group_by(subject, activity) %>%
summarise_each(funs(mean)))
## Check the structure
str(tidytraindata)