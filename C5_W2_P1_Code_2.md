# -----------------------------------------------------------
# Loading and preprocessing the data
# 1. Code for reading in the dataset and/or processing the data
getwd()
setwd("C:/Users/Tom/Documents/COURSERA/C5_ReproducibleResearch/repdata_data_activity")
activity <- read.csv("activity.csv", header = TRUE)

# ----------------------------------------------------------
# What is mean total number of steps taken per day?
# 1. Calculate the total number of steps taken per day
Steps_per_day <- tapply(activity$steps, activity$date, sum)

# 2. Make a histogram of the total number of steps taken each day
hist(Steps_per_day, xlab = "Number of Steps", main = "Steps per Day")

# 3. Calculate and report the mean and median of the total number of steps taken per day
Mean_day <- mean(Steps_per_day, na.rm = TRUE)
Mean_day_2 <- format(Mean_day, digits = 2)
Median_day <- median(Steps_per_day, na.rm = TRUE)
Mean_day_2
Median_day

# ----------------------------------------------------------
# What is the average daily activity pattern?
# 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,
#    averaged across all days (y-axis)
Step_Interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(Step_Interval)), 
     Step_Interval, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity Pattern", 
     type = "l")



# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_Interval <- names(sort(Step_Interval, decreasing = TRUE)[1])
max_Steps <- sort(Step_Interval, decreasing = TRUE)[1]
max_Steps_2 <- format(max_Steps, digits = 2)
max_Interval
max_Steps_2
# ----------------------------------------------------------
# Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset
NA.vals <- sum(is.na(activity$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# I will fill in missing data with the mean number of steps across all days with available data 
# for that particular interval.

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in
Step_Interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# split activity data by interval
activity.split <- split(activity, activity$interval)
# fill in missing data for each interval
for(i in 1:length(activity.split)){
  activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- Step_Interval[i]
}
activity.imputed <- do.call("rbind", activity.split)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
#    median total number of steps taken per day.
Steps_per_day.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
hist(Steps_per_day.imputed, xlab = "Number of Steps", main = "Steps per Day (Imputed data)")

Mean_Day.imputed <- mean(Steps_per_day.imputed, na.rm = TRUE)
Median_Day.imputed <- median(Steps_per_day.imputed, na.rm = TRUE)

# ------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#    indicating whether a given date is a weekday or weekend day.
activity.imputed$day <- ifelse(
                          weekdays(as.Date(activity.imputed$date)) == "Saturday" | 
                          weekdays(as.Date(activity.imputed$date)) == "Sunday", "weekend", "weekday"
                              )

# 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and 
#    the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# Calculate avg steps per interval for weekends
Step_Interval.weekend <- tapply(
                                   activity.imputed[activity.imputed$day == "weekend" ,]$steps, 
                                   activity.imputed[activity.imputed$day == "weekend" ,]$interval, 
                                   mean, 
                                   na.rm = TRUE)

# Calculate avg steps per interval for weekdays
Step_Interval.weekday <- tapply(
                                   activity.imputed[activity.imputed$day == "weekday" ,]$steps, 
                                   activity.imputed[activity.imputed$day == "weekday" ,]$interval, 
                                   mean, 
                                   na.rm = TRUE)

# 2 Panel Plot set
par(mfrow=c(1,2))

# Weekday 
plot(as.numeric(names(Step_Interval.weekday)), 
     Step_Interval.weekday, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Weekday Activity Pattern", 
     type = "l")

# Weekend
plot(as.numeric(names(Step_Interval.weekend)), 
     Step_Interval.weekend, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Weekend Activity Pattern", 
     type = "l")
