init<- function() {
#verification required libraries
        if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
        if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
        library(dplyr)
        library(plyr)
        
}

step1<- function() {
#Merges the training and the test sets to create one data set.
        list_files <- list.files("UCI HAR Dataset", full.names=TRUE, recursive = TRUE)
        column_names<-read.table(list_files[3], stringsAsFactors = FALSE)
        res<-read.table(list_files[15], stringsAsFactors = FALSE)
        res<-rbind(res, read.table(list_files[27], stringsAsFactors = FALSE))
        names(res)<-column_names[,2]
        
        col1<-read.table(list_files[14], stringsAsFactors = FALSE)
        col1<- rbind(col1, read.table(list_files[26], stringsAsFactors = FALSE))
        res<-cbind(res, col1)
        names(res)[562]<- "Subject"
        
        col1<-read.table(list_files[16], stringsAsFactors = FALSE)
        col1<- rbind(col1, read.table(list_files[28], stringsAsFactors = FALSE))
        res<-cbind(res, col1)
        names(res)[563]<- "Activity"
        res<-tbl_df(res)
        res   
}

step2<-function(source){
#Extracts only the measurements on the mean and standard deviation for each measurement.
        index <- (names(source)[(grepl("mean",names(source)) | 
                grepl("std",names(source)) | grepl("Subject",names(source)) | 
                        grepl("Activity",names(source))) ==
                                  TRUE])
        source<-source[, index]
}

step3<- function (source){
#Uses descriptive activity names to name the activities in the data set
        descript<-read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
        names(descript)[1]="Activity"
        names(descript)[2]="FullName.Activity"
        res<- inner_join(source, descript)
        res<-res[,-81]
}

step5<- function(source){
#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
        res2=melt(source, id.var=c("Subject", "FullName.Activity"))
        dmg=group_by(res2, c("Subject", "FullName.Activity"), variable)
        res2 <- dcast(dmg, Subject + FullName.Activity ~ variable, mean, value.var="value")
}

steps<- function(){
      init()
      a<-step1()
      b<-step2(a)
      c<-step3(b)
      d<-step5(с)
      head(d,15)
}