library(plyr)

#create the directory and download the data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
#unzip the data
unzip(zipfile="./data/Dataset.zip",exdir="./data")
files_path=file.path("./data","UCI HAR Dataset")
files<-list.files(files_path,recursive=TRUE)
files


#1.Merges the training and the test sets to create one data set.

#read the Activity,Subject and Features files
activity <- read.table(file.path(files_path,"activity_labels.txt"), header=FALSE, stringsAsFactors=FALSE)
y_test<-read.table(file.path(files_path,"test","Y_test.txt"),header=FALSE,stringsAsFactors=FALSE, col.names=c("activity"))
y_train <- read.table(file.path(files_path,"train", "Y_train.txt"), header=FALSE, stringsAsFactors=FALSE, col.names=c("activity"))
subject_train <- read.table(file.path(files_path, "train", "subject_train.txt"),header = FALSE,stringsAsFactors=FALSE, col.names=c("subjectId"))
subject_test  <- read.table(file.path(files_path, "test" , "subject_test.txt"),header = FALSE,stringsAsFactors=FALSE, col.names=c("subjectId"))
x_test  <- read.table(file.path(files_path, "test" , "X_test.txt" ),header = FALSE,stringsAsFactors=FALSE)
x_train <- read.table(file.path(files_path, "train", "X_train.txt"),header = FALSE,stringsAsFactors=FALSE)
featureNames <- read.table(file.path(files_path,"features.txt"), header=FALSE,stringsAsFactors=FALSE)

#merging the data 
train<-cbind(y_train,subject_train,x_train)
test<-cbind(y_test,subject_test,x_test)
data<-rbind(train,test)

#2.Extract only the measurements on mean and standard deviation for each measurement
measurements<-data[,c(c(1:2),grep(".*mean|std.*",featureNames$V2)+2)]
names<-grep(".*mean|std.*",featureNames$V2,value=TRUE)

#3.Uses descriptive activity names to name the activities in the data set
measurements$activity<-as.factor(measurements$activity)
levels(measurements$activity)<-activity$V2

#4.Appropriately labels the data set with descriptive variable names.
names(measurements)<-c("activity","subjectid", names)

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy<-ddply(measurements, c(.(activity),.(subjectid)),summarize,Means=colMeans(measurements[3:(length(names(measurements)))]))
activityNames<-rep( names(measurements[,3:(length(names(measurements)))]),nrow(activity)*length(c(unique(subject_train$subjectid),unique(subject_test$subjectid))))
tidy$measuredfeature<-activityNames
write.table(tidy, "tidy.txt",row.names=FALSE)
