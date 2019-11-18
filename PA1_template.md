---
title: "PA1_template"
author: "Tom"
date: "November 18, 2019"
output: 
html_document: 
keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# **Reproducible Data, Project 1**

```{r load_data, echo=TRUE}
getwd()
setwd("C:/Users/Tom/Documents/COURSERA/C5_ReproducibleResearch/repdata_data_activity")
activity <- read.csv("activity.csv", header = TRUE)
```



## *What is mean total number of steps taken per day?*
1. *Calculate the total number of steps taken per day*

```{r}
Steps_per_day <- tapply(activity$steps, activity$date, sum)
```
The total number of steps taken per day is `r Steps_per_day`

2. *Make a histogram of the total number of steps taken each day*
``` {r hist1}
hist(Steps_per_day, xlab = "Number of Steps", main = "Steps per Day")
```

3. *Calculate and report the mean and median of the total number of steps taken per day*
``` {r mean_median}
Mean_day <- mean(Steps_per_day, na.rm = TRUE)
Mean_day_2 <- format(Mean_day, digits = 2)
Median_day <- median(Steps_per_day, na.rm = TRUE)

```
The mean number of steps per day is **`r Mean_day_2`** and the median number of steps per day is **`r Median_day`**.

Mean: 10766 
Median: 10765



## *What is the average daily activity pattern?*
1. *Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*
``` {r time_series}
Step_Interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(Step_Interval)), 
     Step_Interval, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity Pattern", 
     type = "l")
```

2. *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*
``` {r max_calc}
max_Interval <- names(sort(Step_Interval, decreasing = TRUE)[1])
max_Steps <- sort(Step_Interval, decreasing = TRUE)[1]
max_Steps_2 <- format(max_Steps, digits = 2)
```
The **`r max_Interval`** contains the max number of steps which are **`r max_Steps_2`**

Max Interval: 835
Max Steps: 206


## *Imputing missing values*
1. *Calculate and report the total number of missing values in the dataset*
``` {r missing_val}
NA.vals <- sum(is.na(activity$steps))
```
Missing Value Count: **`r NA.vals`**

Missing Values: 2304

2. *Devise a strategy for filling in all of the missing values in the dataset.* 
I will fill in missing data with the mean number of steps across all days with available data 
for that particular interval.

The missing data will be filled in with the mean number of steps from all days with availavble data per respective interval

3. *Create a new dataset that is equal to the original dataset but with the missing data filled in*
``` {r new_data}
Step_Interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

# Acivity split by the data interval
activity.split <- split(activity, activity$interval)

# Missing data filled in for each respective interval
for(i in 1:length(activity.split)){
  activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- Step_Interval[i]
}

activity.imputed <- do.call("rbind", activity.split)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]
```

4. *Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*
``` {r histogram_2}
Steps_per_day.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
hist(Steps_per_day.imputed, xlab = "Number of Steps", main = "Steps per Day (Imputed data)")

Mean_Day.imputed <- mean(Steps_per_day.imputed, na.rm = TRUE)
Median_Day.imputed <- median(Steps_per_day.imputed, na.rm = TRUE)
```



## *Are there differences in activity patterns between weekdays and weekends?*

1. *Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*
``` {r new_factor}
activity.imputed$day <- ifelse(
                          weekdays(as.Date(activity.imputed$date)) == "Saturday" | 
                          weekdays(as.Date(activity.imputed$date)) == "Sunday", "weekend", "weekday"
                              )
```

2. *Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*
``` {r panel_plot}
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
```

