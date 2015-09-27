#####################################################
#You should create one R script called run_analysis.R that does the following. 

#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##################################################################################################
require(plyr)

# Directories and files
dataPlace            <- "./UCI_HAR_Dataset/"
featureFile         <- paste(dataPlace, "/features.txt", sep = "")
activity_labelsFile <- paste(dataPlace, "/activity_labels.txt", sep = "")
x_trainFile         <- paste(dataPlace, "/train/X_train.txt", sep = "")
y_trainFile         <- paste(dataPlace, "/train/y_train.txt", sep = "")
subject_trainFile   <- paste(dataPlace, "/train/subject_train.txt", sep = "")
x_testFile          <- paste(dataPlace, "/test/X_test.txt", sep = "")
y_testFile          <- paste(dataPlace, "/test/y_test.txt", sep = "")
subject_testFile    <- paste(dataPlace, "/test/subject_test.txt", sep = "")

# Load raw data
features        <- read.table(featureFile, colClasses = c("character"))
activity_labels <- read.table(activity_labelsFile, col.names = c("ActivityId", "Activity"))
x_train         <- read.table(x_trainFile)
y_train         <- read.table(y_trainFile)
subject_train   <- read.table(subject_trainFile)
x_test          <- read.table(x_testFile)
y_test          <- read.table(y_testFile)
subject_test    <- read.table(subject_testFile)

#Training and the test sets are Merged to create one data set.# Binding sensor data

training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data     <- cbind(cbind(x_test, subject_test), y_test)
sensor_data          <- rbind(training_sensor_data, test_sensor_data)
sensor_labels      <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

#Extracts only the measurements on the mean and standard deviation for each measurement.

sensor_data_mean <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

#Uses descriptive activity names to name the activities in the data set

sensor_data_mean <- join(sensor_data_mean, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean <- sensor_data_mean[,-1]

#labels the data set with descriptive names.

names(sensor_data_mean) <- gsub('\\(|\\)',"",names(sensor_data_mean), perl = TRUE)
names(sensor_data_mean) <- make.names(names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Acc',"Acceleration",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Mag',"Magnitude",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('^t',"TimeDomain.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('\\.mean',".Mean",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean))
names(sensor_data_mean) <- gsub('Freq$',"Frequency",names(sensor_data_mean))

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensorAvgByActSub = ddply(sensor_data_mean, c("Subject","Activity"), numcolwise(mean))
write.table(sensorAvgByActSub, file = "sensorAvgByActSub.txt")
