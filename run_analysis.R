## assuming zip file was extracted into working directory

##loading test dataset
feat<-read.table("features.txt")
act<-read.table("activity_labels.txt")
subtest<-read.table("test/subject_test.txt")
xtest<-read.table("test/X_test.txt")
ytest<-read.table("test/y_test.txt")
colnames(xtest)<-feat$V2
xtest$Subject<-subtest$V1
xtest$Activity<-ytest$V1
accx<-read.table("test/Inertial Signals/total_acc_x_test.txt")

##loading train dataset
subtrain<-read.table("train/subject_train.txt")
xtrain<-read.table("train/X_train.txt")
ytrain<-read.table("train/y_train.txt")
colnames(xtrain)<-feat$V2
xtrain$Subject<-subtrain$V1
xtrain$Activity<-ytrain$V1



#################################################################################
##Step 1. Merging training and test datasets
##using unique(subtrain) and unique(subtest), I've determined that no subjects are included in both datasets
## therefore, using merge() function is unnecessary, can simply bind datasets together

library(dplyr)
testtrain<-bind_rows(xtest, xtrain)


#################################################################################
##Step 2. Extracting relevant columns

## generate list of column numbers of interest:  
## "Activity" column, "Subject" column, all columns containing "mean()" and "std()"

colsinter<-c(grep("Activity", colnames(testtrain)), grep("Subject", colnames(testtrain)), grep("mean\\()",colnames(testtrain)), grep("std\\()", colnames(testtrain)))

##Create dataset containing only columns of interest
tidycomb<-testtrain[,colsinter]

#################################################################################
##Step 3. Replace activity names with description

tidycomb$Activity<-sub("1", "WALKING", tidycomb$Activity)
tidycomb$Activity<-sub("2", "WALKING_UPSTAIRS", tidycomb$Activity)
tidycomb$Activity<-sub("3", "WALKING_DOWNSTAIRS", tidycomb$Activity)
tidycomb$Activity<-sub("4", "SITTING", tidycomb$Activity)
tidycomb$Activity<-sub("5", "STANDING", tidycomb$Activity)
tidycomb$Activity<-sub("6", "LAYING", tidycomb$Activity)

#################################################################################
##Step 4. Change variable names

nmes<-colnames(tidycomb) ## save column names to variable

##modify variable to get more descriptive names
nmes<-sub("^t", "time-", nmes)
nmes<-sub("^f", "frequency-", nmes)
nmes<-sub("BodyAcc", "body-acceleration-", nmes)
nmes<-sub("GravityAcc", "gravity-acceleration-", nmes)
nmes<-sub("BodyGyro", "body-gyroscope-", nmes)
nmes<-sub("Mag", "magnitude-", nmes)
nmes<-sub("Jerk", "jerk-", nmes)
nmes<-sub("X$", "-X-axis", nmes)
nmes<-sub("Y$", "-Y-axis", nmes)
nmes<-sub("Z$", "-Z-axis", nmes)
nmes<-sub("std", "stdev", nmes)
nmes<-gsub("\\(|\\)", "", nmes)
nmes<-sub("Bodybody", "body", nmes)
nmes<-sub(" ", "-", nmes)
nmes<-gsub("--", "-", nmes)

colnames(tidycomb)<-nmes


#################################################################################
##Step 5. Create and save summary dataset

averaged<-tidycomb%>%group_by(Subject, Activity)%>%summarise_each(funs(mean))
write.table(averaged, "tidy.txt", row.name=FALSE)
