

library(plyr)
library(dplyr)
library(reshape2)


    # Load: activity labels
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

    # Load: data column names
    features <- read.table("./UCI HAR Dataset/features.txt")[,2]

    # Read data
    x_train = read.table("./UCI HAR Dataset/train/X_train.txt")
    y_train = read.table("./UCI HAR Dataset/train/y_train.txt")
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    
    x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")    

# 3. Uses descriptive activity names to name the activities in the data set
    names(x_train) = features
    names(x_test) = features

# 4. Appropriately labels the data set with descriptive activity names.
    y_train[,2] = activity_labels[y_train[,1]]
    names(y_train) = c("Activity_ID", "Activity_Label")
    names(subject_train) = "Subject"

    y_test[,2] = activity_labels[y_test[,1]]
    names(y_test) = c("Activity_ID", "Activity_Label")
    names(subject_test) = "Subject"

 
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    extract_features <- grepl("mean|std", features)
    x_train = x_train[,extract_features]
    x_test = x_test[,extract_features]

    # Binding the data
    library(data.table)
    train_data <- cbind(as.data.table(subject_train), y_train, x_train)
    test_data <- cbind(as.data.table(subject_test), y_test, x_test)  
    

# 1. Merges the training and the test sets to create one data set.
    data = rbind(train_data, test_data)


# 5. Creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject.

    id_labels   = c("Subject", "Activity_ID", "Activity_Label")
    data_labels = setdiff(colnames(data), id_labels)
    melt_data   = melt(data, id = id_labels, measure.vars = data_labels)

    # Apply mean function to dataset using dcast function
    tidy_data   = dcast(melt_data, Subject + Activity_Label ~ variable, mean)
    
    write.table(tidy_data, file = "./tidy_data.txt", row.names=FALSE)







