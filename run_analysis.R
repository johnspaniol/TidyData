# Read in the data from files
Features <-  read.table('./RData/UCI HAR Dataset/features.txt',header=FALSE)
ActivityType <-  read.table('./RData/UCI HAR Dataset/activity_labels.txt',header=FALSE)
SubjectTrain <-  read.table('./RData/UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain <-  read.table('./RData/UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain <-  read.table('./RData/UCI HAR Dataset/train/y_train.txt',header=FALSE)

colnames(xTrain) <- Features[,2]

colnames(yTrain) <- "ActivityId"

colnames(SubjectTrain) <- "SubjectId"

colnames(ActivityType) <- c('ActivityId','ActivityType')

TRCombineData <- cbind(yTrain,SubjectTrain,xTrain)
  SubjectTest <- read.table('./RData/UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest <- read.table('./RData/UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest <- read.table('./RData/UCI HAR Dataset/test/y_test.txt',header=FALSE)

# Assign column names
colnames(SubjectTest) <- "SubjectId"
colnames(xTest) <- Features[,2]
colnames(yTest) <- "ActivityId"

TestData <- cbind(yTest,SubjectTest,xTest)

FinalData <- rbind(TRCombineData,TestData)
colNames <- colnames(FinalData)

logicalVector <- (grepl("Activity..",colNames) |
                  grepl("Subject..",colNames) |
                  grepl("-mean..",colNames) &
                 !grepl("-meanFreq..",colNames) &
                 !grepl("mean..-",colNames) |
                  grepl("-std..",colNames) &
                 !grepl("-std()..-",colNames))

FinalData <- FinalData[logicalVector==TRUE]
FinalData <- merge(FinalData,ActivityType,by='ActivityId',all.x=TRUE)
colNames  <- colnames(FinalData)

for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StandardDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","Time",colNames[i])
  colNames[i] <- gsub("^(f)","Frequency",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(FinalData) <- colNames;

FinalDataNoActivityType  <- FinalData[,names(FinalData) != 'ActivityType'];

### Summarizing the FinalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData <- aggregate(FinalDataNoActivityType[,names(FinalDataNoActivityType)!= c('ActivityId','SubjectId')],by=list(ActivityId=FinalDataNoActivityType$ActivityId,SubjectId = FinalDataNoActivityType$SubjectId),mean)

# Merging the tidyData with ActivityType to include descriptive acitvity names
tidyData <- merge(tidyData,ActivityType,by='ActivityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')