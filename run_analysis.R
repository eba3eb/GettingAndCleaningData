# Read in training data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

# Read in testing data
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

# Concatenate training x and y variables into a data frame
train.data <- c(x_train, y_train)
train.data <- as.data.frame(train.data)

# Concatenate testing x and y variables into a data frame
test.data <- c(x_test, y_test)
test.data <- as.data.frame(test.data)

# Concatenate training and testing data
dataset <- rbind(train.data, test.data)

# Read in activity labels and feature labels
activity.labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

# Get only measurements concerned with mean and standard deviation
mean.std.only <- subset(features, grepl("mean\\(\\)", features[,2]) | grepl("std\\(\\)", features[,2]))
indices <- mean.std.only[,1]
# Get indices in format for dataset
indices <- paste("V", indices, sep="")
# Concatenate indices with y variable name
indices <- append(indices, "V1.1")

# Get only data concerned with mean and standard deviation
dataset.mean.std <- dataset[,(names(dataset) %in% indices)]

# Iterate through dataset to map activity number to descriptive label
for (ii in 1:nrow(dataset.mean.std)) {
  activity <- as.numeric(dataset.mean.std[ii, ncol(dataset.mean.std)])
  label <- toString(activity.labels[activity,2])
  dataset.mean.std[ii, ncol(dataset.mean.std)] <- label
}

# Set column names for dataset
names(dataset.mean.std) <- mean.std.only[,2]
# Set y value column name
colnames(dataset.mean.std)[ncol(dataset.mean.std)] <- "activity_label"

# Read in subject data for training and testing
subject.train <- read.table("train/subject_train.txt")
subject.test <- read.table("test/subject_test.txt")
subjects <- rbind(subject.train, subject.test)

# Create more descriptive subject names
subject.names <- rep("subject_", 30)
subject.names <- paste(subject.names, 1:30, sep="")

# Get row names for dataset from x variables
row.names.tidy.set <- colnames(dataset.mean.std)[1:ncol(dataset.mean.std)-1]
# Allocate for row names
full.row.names <- character()

# Set up row names vector to concatenate x variables with y variables
for (ii in 1:length(row.names.tidy.set)) {
  temp.row.names <- character();
  for (jj in 1:nrow(activity.labels)) {
    temp.row.names <- append(temp.row.names, paste(activity.labels[jj,2], row.names.tidy.set[[ii]], sep="."))
  }
  full.row.names <- append(full.row.names, temp.row.names)
}

# Allocate data frame with variables as row names and subjects as columns
df <- as.data.frame(matrix(nrow=length(full.row.names), ncol=length(subject.names)))
names(df) <- subject.names
row.names(df) <- full.row.names

# Set values of data frame
for (ii in 1:nrow(df)) {
  for (jj in 1:ncol(df)) {
    feature.split <- strsplit(row.names(df)[ii], "\\.")
    label <- feature.split[[1]][1]
    feature <- feature.split[[1]][2]
    
    data <- subset(dataset.mean.std[[feature]], subjects == jj, dataset.mean.std[,ncol(dataset.mean.std)] == label)
    
    df[ii,jj] <- mean(data)
  }
}
