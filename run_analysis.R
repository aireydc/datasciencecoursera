# setwd()

# clean workspace
rm(list=ls())

#
# Prepare test data
#

X_test = read.table(file="./UCI HAR Dataset/test/X_test.txt")

# 2947 rows by 561 columns
dim(X_test)
str(X_test[,1:10])
summary(X_test[,1:10])

features = read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE, col.names=c("varnum", "feature"))

# fix duplicate labels of features
length(unique(features$feature))
dupfix = which(duplicated(features$feature))
for (i in dupfix) {
    features$feature[i] = paste(features$feature[i] , "_d" , as.character(i), sep="")
}
length(unique(features$feature))

# label X_test with (now) unique features
varlabels = features$feature
names(X_test) = varlabels

y_test = read.table(file="./UCI HAR Dataset/test/y_test.txt", col.names=c("activity"))

#2947 rows by 1 column
dim(y_test)
str(y_test)

activity_labels = read.table(file="./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
labels = activity_labels$V2
labels

y_test$activity = factor(y_test$activity, labels=labels)
str(y_test)

subject_test = read.table(file="./UCI HAR Dataset/test/subject_test.txt", col.name=c("subject"))

#2947 rows by 1 column
dim(subject_test)
str(subject_test)

# put together test data set
test = cbind(subject_test, y_test, X_test)
dim(test)
str(test)

#
# Prepare train data
#

X_train = read.table(file="./UCI HAR Dataset/train/X_train.txt")

names(X_train) = varlabels

# 7352 rows by 561 columns
dim(X_train)
str(X_train[,1:10])
summary(X_train[,1:10])

y_train = read.table(file="./UCI HAR Dataset/train/y_train.txt", col.names=c("activity"))

# 7352 rows by 1 column
dim(y_train)
str(y_train)

y_train$activity = factor(y_train$activity, labels=labels)
str(y_train)

subject_train = read.table(file="./UCI HAR Dataset/train/subject_train.txt", col.name=c("subject"))

# 7352 rows by 1 column
dim(subject_train)
str(subject_train)

# different subjects in train and test
table(subject_test$subject)
table(subject_train$subject)

# put together train data set
train = cbind(subject_train, y_train, X_train)
dim(train)
str(train)

#
# put together total data set
#

alldata = rbind(train, test)
# make subject a factor variable
alldata$subject = factor(alldata$subject)

dim(alldata)
str(alldata[,1:10])

# clean up a bit
rm(activity_labels, subject_test, subject_train, test, train, X_test, X_train, y_test, y_train)

summary(alldata[,1:10])

# find variables that are mean and std using grep
varnums = c(grep("mean", features$feature, ignore.case=TRUE), grep("std", features$feature, ignore.case=TRUE))
# offset because subject and y vars were inserted to cols 1 and 2
varnums_offset2 = varnums + 2
varnums_ordered = sort(varnums_offset2)

# get selected variables for mean and sd from total data
mean_sd_data = alldata[,c(1:2,varnums_ordered)]

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

# clean up
# rm(list=ls())





