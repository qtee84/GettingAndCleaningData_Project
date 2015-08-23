# 1.Merges the training and the test sets to create one data set.

#Load data from source files
test_subjects <- read.table("./test/subject_test.txt", header=FALSE)
test_set <- read.table("./test/X_test.txt", header=FALSE)
test_labels <- read.table("./test/y_test.txt", header=FALSE)
train_subjects <- read.table("./train/subject_train.txt", header=FALSE)
train_set <- read.table("./train/X_train.txt", header=FALSE)
train_labels <- read.table("./train/y_train.txt", header=FALSE)
activity_labels <- read.table("./activity_labels.txt", header=FALSE, stringsAsFactor=FALSE)
features <- read.table("./features.txt", header=FALSE)

#Combine subject, labels and data for test and train data test by column
test <- cbind(test_subjects, test_labels, test_set)
train <- cbind(train_subjects, train_labels, train_set)

#Combine test and train data set by row
df <- rbind(test, train)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std_indices <- c(grep("mean\\(|std",features$V2))
mean_std <- features[mean_and_std_indices,]

df_mean_std <- df[, c(1,2,mean_and_std_indices+2)]

# 3.Uses descriptive activity names to name the activities in the data set
colnames(activity_labels)<-c("Activity","labels")
colnames(df_mean_std)[c(1,2)]<-c("Subject","Activity")

df_mean_std$Activity<- activity_labels[df_mean_std$Activity, 2]

# 4.Appropriately labels the data set with descriptive variable names. 
colnames(df_mean_std) <- c("Subject","Activity",as.character(mean_std$V2))

# 5.From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
tidydata <- aggregate(df_mean_std[-c(1,2)],by=list(Subject=df_mean_std$Subject,Activity=df_mean_std$Activity),FUN="mean")

# Write tidy data set:
write.table(tidydata, file="tidydata.txt", row.name=FALSE)