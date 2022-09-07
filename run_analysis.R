## Load packages
library(tidyverse)
library(data.table)

## Read in data labels
feature_labels <- fread(file="./UCI HAR Dataset/features.txt")
activity_labels <- fread(file="./UCI HAR Dataset/activity_labels.txt")

## Read in training data
train_x <-fread(file="./UCI HAR Dataset/train/X_train.txt")
train_y <- fread(file="./UCI HAR Dataset/train/y_train.txt")
train_subject <- fread(file="./UCI HAR Dataset/train/subject_train.txt")

body_acc_x_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")

body_gyro_x_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_y_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_z_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")

total_acc_x_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
total_acc_y_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
total_acc_z_train <- fread(file="./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")


## Read in test data
test_x <-fread(file="./UCI HAR Dataset/test/X_test.txt")
test_y <- fread(file="./UCI HAR Dataset/test/y_test.txt")
test_subject <- fread(file="./UCI HAR Dataset/test/subject_test.txt")

body_acc_x_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")

body_gyro_x_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")

total_acc_x_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test <- fread(file="./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")


### Rename columns
train_y <- train_y %>% rename("activity_label" = V1)
test_y <- test_y %>% rename("activity_label" = V1)

train_subject <- train_subject %>% rename("subject" = V1)
test_subject <- test_subject %>% rename("subject" = V1)

setnames(train_x, old = colnames(train_x), new = feature_labels$V2)
setnames(test_x, old = colnames(test_x), new = feature_labels$V2)


## Add activity descriptions
train_y <- train_y %>% mutate(activity = as.factor(case_when(
  activity_label == 6 ~ "LAYING",
  activity_label == 5 ~ "STANDING",
  activity_label == 4 ~ "SITTING",
  activity_label == 3 ~ "WALKING_DOWNSTAIRS",
  activity_label == 2 ~ "WALKING_UPSTAIRS",
  activity_label == 1 ~ "WALKING")))

test_y <- test_y %>% mutate(activity = as.factor(case_when(
  activity_label == 6 ~ "LAYING",
  activity_label == 5 ~ "STANDING",
  activity_label == 4 ~ "SITTING",
  activity_label == 3 ~ "WALKING_DOWNSTAIRS",
  activity_label == 2 ~ "WALKING_UPSTAIRS",
  activity_label == 1 ~ "WALKING")))

## Combine training data
training_data <- cbind(train_x, train_y, train_subject)

## Combine test data
test_data <- cbind(test_x, test_y, test_subject)

## merge training and test data to create master dataset
master_data <- rbind(training_data, test_data)

## Select only mean and SD of measurements

#create a vector with the column numbers containing mean and SD as well as activity and subject
selected_features <- grep("std|[Mm]ean", colnames(master_data))
master_data_subset <- master_data %>% select(all_of(c(selected_features, 563:564)))

## Calculate average of the mean and SD for each subject and activity
average_measures <- master_data_subset %>% group_by(activity, subject) %>%
 summarise(across(everything(), list(mean)))
