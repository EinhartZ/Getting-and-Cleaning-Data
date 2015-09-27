
################################
#Get Data, Set Up Working Directory
################################
dataFolder <- "UCI HAR Dataset"

#check if folder exists
print("looking for data folder")
if (!dir.exists(dataFolder)) {
    
    zipFile <- "getdata-projectfiles-UCI HAR Dataset.zip"
    
    #check if zip file already downloaded
    print("folder not found, checking for zip file")
    if(!file.exists(zipFile)) {
        #download the zip file
        print("zip file not found, downloading...")
        zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(urlZip, zipFile, method = "curl")
        print("download complete")
    }
    
    #unzip into folder
    print("unziping...")
    unzip(zipFile)
    print("unzip complete")
}

#change the data folder as working directory
setwd(paste0("./", dataFolder))

################################
#Read Data
################################

#Read in the data files
print("Reading training data")
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")

print("Reading test data")
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

print("Reading activity and feature files")
activity_labels <- read.table("./activity_labels.txt")
features <- read.table("./features.txt")

################################
#Process Data
################################

#1. Merges the training and the test sets to create one data set.

subject_all <- rbind(subject_train, subject_test)
X_all <- rbind(X_train, X_test)
y_all <- rbind(y_train, y_test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
features_names <- make.names(features[,2])
#Retrive the (Mean | Std) columns indeces
meanstdColIndex <- grep("(mean|std)", features_names, value = FALSE)
data <- cbind(subject_all, y_all, X_all[,meanstdColIndex])

#3. Uses descriptive activity names to name the activities in the data set
data[,2] <- factor(data[,2], levels = activity_labels$V1, labels = activity_labels$V2)

#4. Appropriately labels the data set with descriptive variable names.
#Retrive the (Mean | Std) columns names
meanstdColName <- grep("(mean|std)", features_names, value=TRUE)
colnames(data) <- c("subject", "activity", meanstdColName)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- melt(data, id=c("subject", "activity"), measure.vars=meanstdColName)
tidyData <- dcast(tidyData, subject + activity ~ variable, mean)

################################
#Output
################################

write.table(data, file = "result.txt", row.name = FALSE)
