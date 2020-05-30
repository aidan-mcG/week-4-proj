---
title: "README"
author: "aidan-mcg"
date: "5/30/2020"
output: html_document
---

The following is a description of how the code run_analysis.R functions.
First, packages are loaded and the four key data sets(x test, y test, x train, y train) are saved as data tables.
```{r}
rm(list = ls())
library(dplyr)
library(plyr)
library(tidyr)
library(Stack)
library(tidyselect)

x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("Y_train.txt")

```
From here, more housekeeping is done. The remaining neccessary data is read into R, and a few variables are renamed for clarity and for later use of the join() function.
```{r}
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
```
The next step performed by the code is to remove duplicate columns from x_test and x_train. This first step performed using a subset by unique names. Secondly, the select function is fed an argument of grep, selecting only columns with names containing "mean()" or "std()".
```{r}
#Remove duplicate columns, and select only columns containing mean and std   
x_test <- subset(x_test, select=which(!duplicated(names(x_test))))
x_test <- x_test %>%
      select(grep("mean[(][)]|std[(][)]", names(x_test), value = TRUE))
x_train <- subset(x_train, select=which(!duplicated(names(x_train)))) 
x_train <- x_train %>%
      select(grep("mean[(][)]|std[(][)]", names(x_train), value = TRUE))
```
From here, the primary data sets x_test and x_train are joined with the data containing subject codes. In order to give the function join() a shared column to join by, a column named "index" of ascending numbers was first assigned to the data tables in question.
```{r}
index_test <- c(1:2947)
index_train <- c(1: 7352)
x_test$index <- index_test
x_train$index <- index_train
subject_test$index <- index_test
subject_train$index <- index_train
#merge by subject
x_test <- join(x_test, subject_test, by = "index")
x_train <- join(x_train, subject_train, by = "index")
```
Next, the y data tables were joined with activity names, using "code" (a renamed variable denoting the number assigned to the activity) as a common column. The y data table is listed first, meaning that the activity names would be repeated down the much longer y columns.The variable "code" was then removed, as it had done its job.
```{r}
y_test <- join(y_test, activity_labels, by = "code")
y_train <- join(y_train, activity_labels, by = "code")
y_test$code <- NULL
y_train$code <- NULL
```
Now that the two x data tables contained subject information, and the y data tables contained character information on activity, they were ready to be joined. This was again accomplished by joining by the index column.
```{r}
x_test$index <- index_test
x_train$index <- index_train
y_test$index <- index_test
y_train$index <- index_train
#merge activity into main data set by index
x_test <- join(x_test, y_test, by = "index")
x_train <- join(x_train, y_train, by ="index")
```
The data is now ready to be made into one large table. This is accomplished by simply stacking the x and y data tables on top of each other, as they have the same columns. Some housekeeping is also done here; columns containing subject and activity data are shifted to the front using select, and the index column is removed. Additionally, activity information is made lower case.
```{r}
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
```
The following is the most complicated step. To create a new data table from the tidied larger table, a double for loop was utlized. The loop first filters the larger table by subject, and the second loop filters it by activity. The second loop calls the means of each doubly filtered column and saves them to a matrix "databin". The subject code and activity name are likewise stored in vectors "subjectbin" and "activitybin".  
```{r}
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
```
From here, the two vectors and matrix are assembled to create the new data frame. 
```{r}
#add subject and activity back into newly filtered data
databin <- cbind(databin, activitybin)
databin <- cbind(databin, subjectbin)
#create tidy data frame
tidymeans <- as.data.frame(databin)
```
Only housekeeping remains. The subjectbin and activitybin variables have their data shifted to better named variables, the data is reorganized so that subject and activity are once again the first visible columns, and the rows are renamed to denote that each shows the mean for a subject activity pair.
```{r}
#tidy things up
tidymeans$subject <- tidymeans$subjectbin
tidymeans$activity <- tidymeans$activitybin
tidymeans <- tidymeans%>%
      select(c(69, 70, 1:66))
#turn subject and activity into row names, not column values
subjact <- paste("meansfor", tidymeans$subject, tidymeans$activity)
rownames(tidymeans) <- subjact
```









