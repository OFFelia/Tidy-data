library(plyr)
library(reshape2)


## Merges the training and the test sets to create one data set.

rootdir <- "UCI HAR Dataset"
data <- list()

message("loading features.txt")
data$features <- read.table(paste(rootdir, "features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)

message("loading activity_features.txt")
data$activity_labels <- read.table(paste(rootdir, "activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))


message("loading test set")
data$test <- cbind(subject=read.table(paste(rootdir, "test", "subject_test.txt", sep="/"), col.names="Subject"),
                       y=read.table(paste(rootdir, "test", "y_test.txt", sep="/"), col.names="Activity.ID"),
                       x=read.table(paste(rootdir, "test", "x_test.txt", sep="/")))

message("loading train set")
data$train <- cbind(subject=read.table(paste(rootdir, "train", "subject_train.txt", sep="/"), col.names="Subject"),
                        y=read.table(paste(rootdir, "train", "y_train.txt", sep="/"), col.names="Activity.ID"),
                        x=read.table(paste(rootdir, "train", "X_train.txt", sep="/")))

rename.features <- function(col) {
    col <- gsub("tBody", "Time_Body", col)
    col <- gsub("tGravity", "Time_Gravity", col)
    
    col <- gsub("fBody", "FFT_Body", col)
    col <- gsub("fGravity", "FFT_Gravity", col)
    
    col <- gsub("\\-mean\\(\\)\\-", "_Mean.", col)
    col <- gsub("\\-std\\(\\)\\-", "_Std.", col)
    
    col <- gsub("\\-mean\\(\\)", "_Mean", col)
    col <- gsub("\\-std\\(\\)", "_Std", col)
    
    return(col)
}

## Extracts only the measurements on the mean and standard deviation for each measurement.

tidy <- rbind(data$test, data$train)[,c(1, 2, grep("mean\\(|std\\(", data$features$name) + 2)]

## Uses descriptive activity names to name the activities in the data set

names(tidy) <- c("Subject", "Activity.ID", rename.features(data$features$name[grep("mean\\(|std\\(", data$features$name)]))

## Appropriately labels the data set with descriptive activity names.

tidy <- merge(tidy, data$activity_labels, by.x="Activity.ID", by.y="id")
tidy_data <- tidy[,!(names(tidy) %in% c("Activity.ID"))]

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_mean_data <- ddply(melt(tidy, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))

write.csv(tidy_mean_data, file = "tidy_mean_data.txt",row.names = FALSE)
write.csv(tidy_data, file = "tidy_data.txt",row.names = FALSE)