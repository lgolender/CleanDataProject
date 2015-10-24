# prepare the training data 
features     = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/features.txt',header=FALSE);
activityType = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/activity_labels.txt',header=FALSE);
subjectTrain = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/train/subject_train.txt',header=FALSE);
xTrain       = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/train/x_train.txt',header=FALSE);
yTrain       = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/train/y_train.txt',header=FALSE);

colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

trainingData = cbind(yTrain,subjectTrain,xTrain);



# prepare the test data
subjectTest = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/test/subject_test.txt',header=FALSE); 
xTest       = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/test/x_test.txt',header=FALSE);
yTest       = read.table('C:/Leo/HW/CleanData/UCI HAR Dataset/test/y_test.txt',header=FALSE);

colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

testData = cbind(yTest,subjectTest,xTest);


# The complete data set
Data = rbind(trainingData,testData);

colNames  = colnames(Data); 

filterVec = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# filter the data
Data = Data[filterVec == TRUE];
Data = merge(Data,activityType,by='activityId',all.x = TRUE);
dataColNames  = colnames(Data); 


# Clean up
for (i in 1:length(dataColNames)) 
{
  dataColNames[i] = gsub("\\()","",dataColNames[i])
  dataColNames[i] = gsub("-std$","StdDev",dataColNames[i])
  dataColNames[i] = gsub("-mean","Mean",dataColNames[i])
  dataColNames[i] = gsub("^(t)","time",dataColNames[i])
  dataColNames[i] = gsub("^(f)","freq",dataColNames[i])
  dataColNames[i] = gsub("([Gg]ravity)","Gravity",dataColNames[i])
  dataColNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",dataColNames[i])
  dataColNames[i] = gsub("[Gg]yro","Gyro",dataColNames[i])
  dataColNames[i] = gsub("AccMag","AccMagnitude",dataColNames[i])
  dataColNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",dataColNames[i])
  dataColNames[i] = gsub("JerkMag","JerkMagnitude",dataColNames[i])
  dataColNames[i] = gsub("GyroMag","GyroMagnitude",dataColNames[i])
};

colnames(Data) = dataColNames;


DataNoActivityType  = Data[,names(Data) != 'activityType'];
tidyData    = aggregate(DataNoActivityType[,names(DataNoActivityType) != c('activityId','subjectId')],by=list(activityId=DataNoActivityType$activityId,subjectId = DataNoActivityType$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, 'C:/Leo/HW/CleanData/UCI HAR Dataset/tidyData.csv',row.names=TRUE,sep=',')
