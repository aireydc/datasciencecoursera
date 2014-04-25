Getting and Cleaning Data: "run_analysis.R" script
========================================================

This README file describes how the "run_analysis.R" script operates
on the files from [here](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/) to produce the "tidy.txt" tiny data file.

In broad terms, the script (1) combines information from three files
containing test data, (2) combines information from three files
containing training data, (3) combines test and training data into
one total data file, (4) selects a subset of the variables from
the total data file representing mean or standard deviation summaries,
and (5) collapses the data on subject and activity factors to
means for each subject and activity combination, resulting in a tidy
dataset called "tidy.txt". The "CodeBook" file describes the contents 
of the tidy dataset. The remainder of this document fleshes out the
steps above in terms of R code.

```
#########################
# (1) Prepare test data #
#########################

# reads in 2947 rows for 561 measured variables
X_test = read.table(file="./UCI HAR Dataset/test/X_test.txt")

# reads in measured variable names to name X_test columns
features = read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE, col.names=c("varnum", "feature"))

# fixes duplicate labels of features
length(unique(features$feature))
dupfix = which(duplicated(features$feature))
for (i in dupfix) {
    features$feature[i] = paste(features$feature[i] , "_d" , as.character(i), sep="")
}
length(unique(features$feature))

# label X_test with (now) unique features
varlabels = features$feature
names(X_test) = varlabels

# reads in 2947 rows of activity factor
y_test = read.table(file="./UCI HAR Dataset/test/y_test.txt", col.names=c("activity"))

# prepare labels for numeric activity factor levels
activity_labels = read.table(file="./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
labels = activity_labels$V2
labels

# label activity factor levels
y_test$activity = factor(y_test$activity, labels=labels)
str(y_test)

# read in subject column, 2947 rows
subject_test = read.table(file="./UCI HAR Dataset/test/subject_test.txt", col.name=c("subject"))

# put together test data set
test = cbind(subject_test, y_test, X_test)


##########################
# (2) Prepare train data #
##########################

# reads in 7352 rows for 561 measured variables
X_train = read.table(file="./UCI HAR Dataset/train/X_train.txt")

# label X_train variables
names(X_train) = varlabels

# read in activity factor, 7352 rows
y_train = read.table(file="./UCI HAR Dataset/train/y_train.txt", col.names=c("activity"))

# label activity factor levels
y_train$activity = factor(y_train$activity, labels=labels)
str(y_train)

# read in subjects vector, 7352 rows
subject_train = read.table(file="./UCI HAR Dataset/train/subject_train.txt", col.name=c("subject"))

# put together train data set
train = cbind(subject_train, y_train, X_train)
dim(train)
str(train)

###########################
# (3) Bind total data set #
###########################

# different subjects in train and test!
table(subject_test$subject)
table(subject_train$subject)

# bind test and train data
alldata = rbind(train, test)

# make subject a factor variable
alldata$subject = factor(alldata$subject)

# clean up a bit
rm(activity_labels, subject_test, subject_train, test, train, X_test, X_train, y_test, y_train)

##################################
# (4) Subset the total data file #
##################################

# find 86 variables that are mean and std using grep
varnums = c(grep("mean", features$feature, ignore.case=TRUE), grep("std", features$feature, ignore.case=TRUE))

# add offset because subject and y vars were inserted to cols 1 and 2
varnums_offset2 = varnums + 2
varnums_ordered = sort(varnums_offset2)

# get selected variables for mean and sd from total data
mean_sd_data = alldata[,c(1:2,varnums_ordered)]

#############################
# (5) Collapse to tidy data #
#############################

# collapse to means by subject and activity
library(reshape2)

# using all data gives 180 obs and 563 vars
molten1 = melt(alldata, id.vars=c("subject", "activity"), na.rm=TRUE)
melted_means1 = dcast(molten1, subject + activity ~ variable, mean, na.rm=TRUE)

# using data subset gives 180 obs and 88 vars
molten2 = melt(mean_sd_data, id=c("subject", "activity"))
tidy = dcast(molten2, subject + activity ~ variable, mean, na.rm=TRUE)

# save data
save(tidy, file="tidy.RData")
write.csv(tidy, file="tidy.txt")

```
