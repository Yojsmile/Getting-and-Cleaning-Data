<!-- R Commander Markdown Template -->
CodeBook
=======================



This is a code book that describes the variables, the data, and any transformations or work that were performed to clean up the data.

## The data source

* Source of the data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* Full description of the dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## The data

The dataset includes the following files:
- 'activity_labels.txt': Links the class labels with their activity name.
- 'features.txt': List of all features.
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'README.txt'

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis.

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration.

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.


## Transformation details

There are 5 parts:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## How ```run_analysis.R``` implements the above steps:

* Require ```reshapre2``` and ```data.table``` libraries.
* Load both train and test data
* Load the features and activity labels.
* Extract the mean and standard deviation column names and data.
* Merged the data set.
* Create the tidy text file.

