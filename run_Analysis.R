op<-function() {
  
  #filtering out the columns that contain mean and std measure in the features table
  dffeatures=read.table("./UCI HAR Dataset/features.txt")
  feature_mean=dffeatures[grep("-mean()-",dffeatures$V2,fixed=TRUE),]
  feature_std=dffeatures[grep("-std()-",dffeatures$V2,fixed=TRUE),]
  feature_mean_std=rbind(feature_mean,feature_std)
  
  #finding out the column number and column names of the releavnet Mean & std measurements
  col_feature_mean_std=feature_mean_std[,1]
  col_name_feature_mean_std=feature_mean_std[,2]
  
  # filtering relevant columns from the test date and assignging colnames
  dftestx=read.table("./UCI HAR Dataset/test/X_test.txt")
  dftestx=dftestx[,col_feature_mean_std]
  colnames(dftestx)=col_name_feature_mean_std
  
  dftesty=read.table("./UCI HAR Dataset/test/y_test.txt")
  colnames(dftesty)="Activity_ID"
  
  dftestsub=read.table("./UCI HAR Dataset/test/subject_test.txt")
  colnames(dftestsub)="Subject_ID"
  
  dftest=cbind(dftestsub,dftesty,dftestx)
  #dftest["Type"]="Test"
  
  
  # filtering relevant columns from the train date and assignging colnames
  dftrainx=read.table("./UCI HAR Dataset/train/X_train.txt")
  dftrainx=dftrainx[,col_feature_mean_std]
  colnames(dftrainx)=col_name_feature_mean_std
  
  dftrainy=read.table("./UCI HAR Dataset/train/y_train.txt")
  colnames(dftrainy)="Activity_ID"
  
  dftrainsub=read.table("./UCI HAR Dataset/train/subject_train.txt")
  colnames(dftrainsub)="Subject_ID"
  
  dftrain=cbind(dftrainsub,dftrainy,dftrainx)
  #dftrain["Type"]="Train"
  
  
  # creating tidy data by combining test and train data into 1 dataframe
  tidydf=rbind(dftest,dftrain)
  dfactivity=read.table("./UCI HAR Dataset/activity_labels.txt")
  colnames(dfactivity)=c("Activity_ID","Activity_Label")
  
  #merging tidy data with activity table to get the activity label instead of activity ID
  tidydf=merge(dfactivity,tidydf,by.x="Activity_ID",by.y="Activity_ID",all=TRUE)
  tidydf=tidydf[, -1]

  #reshaping data using reshape2 package to get the summary of
  tidydf2=melt(tidydf, id.vars=c("Activity_Label", "Subject_ID"))
  tidydf2<-ddply(tidydf2, c("Activity_Label", "Subject_ID", "variable"), summarise, mean = mean(value,na.rm=TRUE))
  
  #writing data in a txt file
  write.table(tidydf2,file="tidydata.txt", sep=",",row.names = FALSE)
  
