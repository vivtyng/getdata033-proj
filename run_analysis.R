#==============================
# step 1: Merge training and test data
t1d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")   # subject ID [1-30] for 7352 train records 
t2d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")         # activity ID [1-6] for 7352 train records 
t3d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")         # read train data 
u1d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")     # subject ID for 2497 test records 
u2d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")           # activity ID for 2497 test records 
u3d <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")           # read test data 
tmp1 <- cbind(t1d,t2d,t3d)                     # column bind training data
tmp2 <- cbind(u1d,u2d,u3d)                     # column bind test data
BigData <- rbind(tmp1, tmp2)                   # row bind training and test data
### Now add the column names:
featurelist <- read.table("features.txt")      # read names of 561 features
FL <- as.vector(featurelist[,2])               # convert data frame to vector
Lfea <- length(FL)                            
colnames(BigData)[1] <- "Subject"              # manually add subject and activity sol names
colnames(BigData)[2] <- "Activity" 
for (i in 1:Lfea){                             # add 561 feature names 
  colnames(BigData)[i+2] <- FL[i]
}
#====================================
# step 2 Extracts only mean and standard deviation for each measurement. 
coln <- colnames(BigData)
Tsk1 <- grepl("mean()",coln)|grepl("std()",coln) # find 79 cols containing mean() or std() in name
BigData2 <- cbind(BigData[,1:2],BigData[,Tsk1])  # combine subject, activity and 79 columns
#====================================
# step 3 Uses descriptive activity names to name the activities in the data 
actlbl <- read.table("activity_labels.txt")
nlabel <- as.vector(actlbl[,1])
label <- as.vector(actlbl[,2])
BigData3 <- BigData2
for (i in 1:6) {                                # convert step 3, as I will activity codes to char strings in 
                                                # activity_labels.txt
  BigData3$Activity[BigData3$Activity==i] <- label[i]
}
#====================================
# step 4 Appropriately labels the data set with descriptive variable 
# names. 
###       This is already performed towards the end of step 1, 
###       by using entries from features.txt as col names
#====================================
# step 5 From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.
BigData5 <- BigData3
for (i in 1:6) {
  BigData5$Activity[BigData5$Activity==label[i]] <- nlabel[i]  # revert relabel activity by integers
}
groups <- 10*BigData5$Subject+as.numeric(BigData5$Activity)    # Creating an integer index for each entry, with subject ID in 100s and 10s digits, followed by activity ID in ones digit
ugroups <- sort(unique(groups))                                # find unique subject+activity combinations 
NeatData <- data.frame("Subject"=floor(ugroups/10),"Activity"=ugroups %% floor(ugroups/10)) # Manually add the subject and activity IDs for NeatData
for (i in 3:81) {                                   
NeatData <- cbind(NeatData,tapply(BigData5[,i],groups,mean))   # Average each col data [1-79] by group index
colnames(NeatData)[i] <- paste0("<",colnames(BigData5)[i],">") # Add "<>" to col names to denote an average has been taken
}
#====================================
# step 6 output neat data results
write.table(NeatData, file="NeatData.txt",sep=",  ",row.name=FALSE)
