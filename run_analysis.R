rm(list = ls())
library(dplyr)
library(plyr)
library(tidyr)
library(Stack)
library(tidyselect)

#Load test data and feature names
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("Y_train.txt")

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")
#name activity label columns
names(activity_labels) <- c("code", "activity")
names(y_test) <- c("code")
names(y_train) <- c("code")

subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")
#name the subject column "subject"
names(subject_test) <- c("subject")
names(subject_train) <- c("subject")

#Create vector of feature names
names <- as.character(features$V2)

#Name the columns of x_test
names(x_test) <- names
names(x_train) <- names

#Remove duplicate columns, and select only columns containing mean and std   
x_test <- subset(x_test, select=which(!duplicated(names(x_test))))
x_test <- x_test %>%
      select(grep("mean[(][)]|std[(][)]", names(x_test), value = TRUE))
x_train <- subset(x_train, select=which(!duplicated(names(x_train)))) 
x_train <- x_train %>%
      select(grep("mean[(][)]|std[(][)]", names(x_train), value = TRUE))

#add index to each df to match with subject
index_test <- c(1:2947)
index_train <- c(1: 7352)
x_test$index <- index_test
x_train$index <- index_train
subject_test$index <- index_test
subject_train$index <- index_train
#merge by subject
x_test <- join(x_test, subject_test, by = "index")
x_train <- join(x_train, subject_train, by = "index")

#merge activity code with activity name
y_test <- join(y_test, activity_labels, by = "code")
y_train <- join(y_train, activity_labels, by = "code")
y_test$code <- NULL
y_train$code <- NULL

#add indexes to merge by activity code
x_test$index <- index_test
x_train$index <- index_train
y_test$index <- index_test
y_train$index <- index_train
#merge activity into main data set by index
x_test <- join(x_test, y_test, by = "index")
x_train <- join(x_train, y_train, by ="index")

#merge the data by stacking since both dfs are the same width/have same columns
xy <- Stack(x_train, x_test)
xy$index <- NULL
#reorder columns
positions <- c(67, 68, 1:66)
xy <- xy%>%
      select(all_of(positions))
#make activities lower case, create vector of activities for next step
xy <- xy%>%
      mutate(activity = tolower(activity))
activityfilter <- c(unique(xy$activity))

#create new data set with avgs
subjectbin <- character()
activitybin <- character()
databin <- matrix(nrow = 0, ncol = 66)
for (i in 1:30) {
      tempdata1 <- xy%>%
            filter(subject == i)
      j <- i
      for(i in activityfilter) {
            tempdata2 <- filter(tempdata1, activity == i)%>%
                  select(c(3:68))%>%
                  colMeans
            databin <- rbind(databin, tempdata2)
            subjectbin <- c(subjectbin, j)
            activitybin <- c(activitybin, i)
      }
}
#add subject and activity back into newly filtered data
databin <- cbind(databin, activitybin)
databin <- cbind(databin, subjectbin)
#create tidy data frame
tidymeans <- as.data.frame(databin)

#tidy things up
tidymeans$subject <- tidymeans$subjectbin
tidymeans$activity <- tidymeans$activitybin
tidymeans <- tidymeans%>%
      select(c(69, 70, 1:66))
#turn subject and activity into row names, not column values
subjact <- paste(tidymeans$subject, tidymeans$activity)
rownames(tidymeans) <- subjact
tidymeans <- tidymeans%>%
      select(c(3:68))


