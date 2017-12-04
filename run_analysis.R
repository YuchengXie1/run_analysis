library("plyr")
x_train <- read.table("./UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
feature <- read.table("./UCI HAR Dataset/UCI HAR Dataset/features.txt")
feature <- feature[,2]
x_test <- read.table("./UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
x <- rbind(x_train,x_test)   ##step1:merge the dataset
select <- grepl("(.*)[Mm]ean[^Freq](.*)|(.*)[Ss]td(.*)",feature)
selectedfeature <- feature[select]
selectedx <- x[select]  ##step2:Extracts only the measurements on the mean and standard deviation for each measurement.

##step4:Appropriately labels the data set with descriptive variable names.
colnames(selectedx) <- selectedfeature  
names(selectedx) <- gsub("\\(", "", names(selectedx))
names(selectedx) <- gsub("\\)", "", names(selectedx))# remove "()"
names(selectedx) <- gsub("mean", "Mean", names(selectedx)) # capitalize M
names(selectedx) <- gsub("std", "Std", names(selectedx)) # capitalize S
names(selectedx) <- gsub("-", "", names(selectedx)) # remove "-" in column names 
##step3:Uses descriptive activity names to name the activities in the data set
y <- rbind(y_train,y_test)
y <- as.numeric(y)
Y <- NULL
for(i in 1:10299){
  if (y[i,1]==1){a<-"WALKING"}
  if (y[i,1]==2){a<-"WALKING_UPSTAIRS"}
  if (y[i,1]==3){a<-"WALKING_DOWNSTAIRS"}
  if (y[i,1]==4){a<-"SITTING"}
  if (y[i,1]==5){a<-"STANDING"}
  if (y[i,1]==6){a<-"LAYING"}
  Y<-rbind(Y,a)
}
cleandata <- cbind(Y,selectedx)  
names(cleandata)[1] = "activity"
spldata <- split(cleandata,cleandata$activity) ##step5:creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean <- NULL
for (i in 1:6){
  activity <- spldata[i]
  activitydataframe <- as.data.frame(activity)
  act.mean <- sapply(activitydataframe,mean)
  mean <- cbind(mean,act.mean)
}
mean1 <- mean[2:74,]
rownames(mean1) = colnames(cleandata)[2:74]

write.table(mean1,"./run",row.name=FALSE) ###output
