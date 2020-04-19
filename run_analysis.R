# Title:              run_analysis.R
# Author:             Ellen Quarles
# Date Submitted:     2020-04-13
#-----------------------------------------------------------------------------------
# Overview: This script is designed to use the UCI HAR Dataset and:
# A) Merge the training and the test sets to create one data set.
# B) Apply the feature names as the column names.
# C) Extract only the measurements on the mean and standard deviation for each measurement.
# D) Use descriptive activity names to name the activities in the data set.
# E) Appropriately label the data set with descriptive variable names.
# F) From the data set in step 4, create a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
#-----------------------------------------------------------------------------------


###########################################################################
##### A) Merge the training and the test sets to create one data set. #####
###########################################################################

# Set wd to UCI dataset folder and read in all necessary tables
setwd("C:/Users/Ellen/Desktop/R_specialization_course/Getting and cleaning data course/data/UCI HAR Dataset/")

X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
sub_test <- read.table("./test/subject_test.txt")

X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
sub_train <- read.table("./train/subject_train.txt")


# Perform basic data exploration to ensure data are in format expected:

# These should be one column of integers 2:24
str(sub_test)
summary(sub_test)
# These should be one column of integers 1:30
str(sub_train)
summary(sub_train)

# These should be one column of integers 1:6
str(y_test)
summary(y_test)
str(y_train)
summary(y_train)


# Combine the y_test and sub_test as extra columns in front of X_test.
library(tidyr)
library(dplyr)

names(y_test) <- c("ActivityCode")
names(sub_test) <- c("SubjectCode")
test1 <- bind_cols(sub_test, y_test, X_test)
head(names(test1))

names(y_train) <- c("ActivityCode")
names(sub_train) <- c("SubjectCode")
train1 <- bind_cols(sub_train, y_train, X_train)
head(names(train1))



# Combine the test and train datasets.
# Clean environment of now-unneeded tables.
data <- bind_rows(train1, test1)
rm(X_train, y_train, sub_train, X_test, y_test, sub_test, test1, train1)



###########################################################
##### B) Apply the feature names as the column names. #####
###########################################################

### Read in, and explore, the files features.txt
features <- read.table("./features.txt")
# This should be a data frame with 561 rows and 2 columns (integer, factor name of feature)
str(features)
head(table(features[,2]))


# It might be helpful to work with these as character class rather than factor class.
# Let's transform the V2 column into a class(character) column.
# Since there were more features than factors, this means there were some repeat
# feature names. Let's fix that.
features <- transmute(features, V1 = V1, feats = as.character(V2))
features$feats <- make.unique(features$feats)
head(features)
#   V1             feats
# 1  1 tBodyAcc-mean()-X
# 2  2 tBodyAcc-mean()-Y
# 3  3 tBodyAcc-mean()-Z
# 4  4  tBodyAcc-std()-X
# 5  5  tBodyAcc-std()-Y
# 6  6  tBodyAcc-std()-Z


# Since R can do weird things with column names that start with numbers, let's see if any of these do:
testlist <- grepl("^[a-z]", features$feats)
sum(testlist) # 561
testlist <- grepl("^[0-9]", features$feats)
sum(testlist) # 0
rm(testlist)
# Nope, all start with letters. Good.


# So, add in the features as column names in the combined dataset (data).
newcols <- c("SubjectCode", "ActivityCode", features$feats)
names(data) <- newcols
rm(features, newcols)


# Save the data at this point just in case. (Optional - my personal habit.)
write.csv(data, "./combinedTestTrain01.csv", row.names=FALSE)



#####################################################################################################
##### C) Extract only the measurements on the mean and standard deviation for each measurement. #####
#####################################################################################################

# For this, I'll need to see which columns have either "mean()" or "std()".
library(stringr)

colsOfInterest.mean <- grep("-mean[[:punct:]]", names(data), value=TRUE)# 33 cols
colsOfInterest.stds <- grep("-std[[:punct:]]", names(data), value=TRUE) # 33 cols

colsOfInterest <- c(colsOfInterest.mean, colsOfInterest.stds)

# Create new table with only the columns with codes and the colsOfInterest vector of col names.
# Then clean environment again.
mnsd <- select(data, "SubjectCode", "ActivityCode", contains(colsOfInterest))
rm(colsOfInterest, colsOfInterest.mean, colsOfInterest.stds, data)



#####################################################################################
##### D) Use descriptive activity names to name the activities in the data set. #####
#####################################################################################

# Read in activities codes and lables table.
activities <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)
activities
#   V1                 V2
# 1  1            WALKING
# 2  2   WALKING_UPSTAIRS
# 3  3 WALKING_DOWNSTAIRS
# 4  4            SITTING
# 5  5           STANDING
# 6  6             LAYING


# Replace the activity codes with the activity, then rearrange the columns.
# I'm using case_when instead of an apply function simply to make the changes to ActivityCode explicit
# and readable. This is fine with such a short list of codes.
mnsd <- mnsd %>%
               mutate(
                    Activity=case_when(  # 
                         ActivityCode == 1 ~ "walking",
                         ActivityCode == 2 ~ "walkingupstairs",
                         ActivityCode == 3 ~ "walkingdownstairs",
                         ActivityCode == 4 ~ "sitting",
                         ActivityCode == 5 ~ "standing",
                         ActivityCode == 6 ~ "laying")) %>%
     select(SubjectCode, Activity, 3:68)
rm(activities)



###############################################################################
#### E) Appropriately label the data set with descriptive variable names. #####
###############################################################################

## I noticed that all those column names can be broken up into multiple categories:
# time vs frequency
# Body vs Gravity
# Acc vs Gyro
# jerk vs "" vs mag vs jerkmag
# X/Y/Z/""
# mean vs sd

## mnsd.orig <- mnsd # so that if I mess up mnsd I can recover the information quickly.
## mnsd <- mnsd.orig



## This is long, but each step is commented to ease understanding.
## All of this is piped to keep the number of intermediate data tables to a minimum
## and keep processing time short.
mnsd.final <- mnsd %>% 
     
     # Make the data table long format, with all colnames as values in column var1
     pivot_longer(cols=-c(1:2),
                  names_to = "var1",
                  values_to = "Measurement") %>%
     
     # var1 is separatable by the first "-" into a new complex term in Var2 
     # and the mean() or std() values in SummaryStatistic and X/Y/Z/"" in Axis cols.
     # Remove the "()" part of mean() and std().
     separate(col = var1,
              sep = "-",
              into = c("Var2", "SummaryStatistic", "Axis")) %>%
     mutate(SummaryStatistic = gsub("mean\\(\\)", "Mean", SummaryStatistic),
            SummaryStatistic = gsub("std\\(\\)", "StandardDeviation", SummaryStatistic)) %>%
     
     # Var2 is tricky to separate at this point unless there is a space between the first letter
     # which denotes whether the data are time related or Fourier transformed frequency data.
     # This code piece adds that space, and removes a typo where "Body" sometimes in entered
     # as "BodyBody".
     mutate(Var2 = gsub("^t", "t ", Var2),
            Var2 = gsub("^f", "f ", Var2),
            Var2 = gsub("BodyBody", "Body", Var2)) %>%
     
     # Now the t and f parts of Var2 can be housed in SignalDomain,
     # as whole words thanks to case_when(), 
     # and the remainder of the feature column is in Var3.
     separate(col = Var2,
              sep = " ",
              into = c("SignalDomain", "Var3")) %>%
     mutate(SignalDomain=case_when(
          SignalDomain == "t" ~ "time",
          SignalDomain == "f" ~ "frequency")) %>%
     
     # Adding a space after Body and Gravity allow us to easily separate
     # these AccelerationComponent variables from the sensor and derived values of Var4.
     mutate(Var3 = gsub("^Body", "body ", Var3),
            Var3 = gsub("^Gravity", "gravity ", Var3)) %>%
     separate(col = Var3,
              sep = " ",
              into = c("AccelerationComponent", "Var4")) %>%
     
     # Add a space again, this time to separate Acc and Gyro, rename them into whole words,
     # and store them in the column Sensor. The remaining string from Var4 is now stored in derivedValue.
     mutate(Var4 = gsub("^Acc", "Acc ", Var4),
            Var4 = gsub("^Gyro", "Gyro ", Var4)) %>%
     separate(col = Var4,
              sep = " ",
              into = c("Sensor", "DerivedValue")) %>%
     mutate(Sensor=case_when(
          Sensor == "Acc" ~ "accelerometer",
          Sensor == "Gyro" ~ "gyroscope"))



## Check the final table (tibble, now) to make sure all variables are coded correctly.
str(mnsd.final)
# tibble [679,734 x 9] (S3: tbl_df/tbl/data.frame)
#  $ SubjectCode          : int [1:679734] 1 1 1 1 1 1 1 1 1 1 ...
#  $ Activity             : chr [1:679734] "standing" "standing" "standing" "standing" ...
#  $ SignalDomain         : chr [1:679734] "time" "time" "time" "time" ...
#  $ AccelerationComponent: chr [1:679734] "body" "body" "body" "gravity" ...
#  $ Sensor               : chr [1:679734] "accelerometer" "accelerometer" "accelerometer" "accelerometer" ...
#  $ derivedValue         : chr [1:679734] "" "" "" "" ...
#  $ SummaryStatistic     : chr [1:679734] "mean" "mean" "mean" "mean" ...
#  $ Axis                 : chr [1:679734] "X" "Y" "Z" "X" ...
#  $ Measurement          : num [1:679734] 0.2886 -0.0203 -0.1329 0.9634 -0.1408 ...

rm(mnsd)



#####################################################################################
##### F) From the data set in step 4, create a second, independent tidy data    #####
##### set with the average of each variable for each activity and each subject. #####
#####################################################################################

# Using mnsd.final...
mnsd.tidy <- mnsd.final %>% 
     
     #...group all variables that aren't Measurement...
     group_by(SubjectCode, Activity, SignalDomain, AccelerationComponent, 
              Sensor, DerivedValue, SummaryStatistic, Axis) %>%
     
     #...and find the mean of Measurement for each of those groups...
     summarize_at(c("Measurement"), mean) %>%
     
     #...and finally end with a column for mean(mean) and mean(StandardDeviation).
     pivot_wider(names_from = SummaryStatistic, values_from = Measurement)

     
# Change column names for the values derived above.
names(mnsd.tidy)
newnames <- c("SubjectCode","Activity","SignalDomain","AccelerationComponent",
              "Sensor","DerivedValue","Axis", "meanOfMean", "meanOfStandardDeviation")
names(mnsd.tidy) <- newnames



head(mnsd.tidy) # 5940 obs of 9 variables
# # A tibble: 6 x 9
# # Groups:   SubjectCode, Activity, SignalDomain, AccelerationComponent, Sensor, DerivedValue [2]
#   SubjectCode Activity SignalDomain AccelerationComponent Sensor       DerivedValue Axis  meanOfMean meanOfStandardDeviat~
#         <int> <chr>    <chr>        <chr>                 <chr>        <chr>        <chr>      <dbl>                 <dbl>
# 1           1 laying   frequency    body                  acceleromet~ ""           X         -0.939                -0.924
# 2           1 laying   frequency    body                  acceleromet~ ""           Y         -0.867                -0.834
# 3           1 laying   frequency    body                  acceleromet~ ""           Z         -0.883                -0.813
# 4           1 laying   frequency    body                  acceleromet~ "Jerk"       X         -0.957                -0.964
# 5           1 laying   frequency    body                  acceleromet~ "Jerk"       Y         -0.922                -0.932
# 6           1 laying   frequency    body                  acceleromet~ "Jerk"       Z         -0.948                -0.961



# Save tidy dataset as per instructed by course materials (write.table, row.names=FALSE)
write.table(file="./tidydataset.txt", mnsd.tidy, row.names=FALSE)

