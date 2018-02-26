---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
setwd("C:/~/Coursera/DataScience/Course5_ReproducibleResearch/Week2/Project")
raw <- data.table::fread("activity.csv")


## What is mean total number of steps taken per day?
stepsByDay <- raw[, c(lapply(.SD, sum, na.rm = TRUE)), .SDcols = c("steps"), by = .(date)]

hist(stepsByDay$steps, xlab = "Steps", ylab = "Freq")

stepsByDay[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]

## What is the average daily activity pattern?

int <- raw[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]

library(ggplot2)
ggplot(int, aes(x = interval , y = steps)) + geom_line(color="black") + labs(title = "Time Series Plot", x = "int$interval", y = "int$steps")

int[steps == max(steps), .(max_interval = interval)]

## Imputing missing values
raw[is.na(steps), .N ]

raw[is.na(steps), "steps"] <- raw[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

data.table::fwrite(x = raw, file = "activityFilled.csv")

steps <- raw[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

ggplot(steps, aes(x = steps)) + geom_histogram(fill = "black") + labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

## Are there differences in activity patterns between weekdays and weekends?

raw <- data.table::fread("activity.csv")
raw[, date := as.POSIXct(date, format = "%Y-%m-%d")]
raw[, `DayOfWeek`:= weekdays(x = date)]
raw[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `DayOfWeek`), "DayType"] <- "Weekday"
raw[grepl(pattern = "Saturday|Sunday", x = `DayOfWeek`), "DayType"] <- "Weekend"
raw[, `DayType` := as.factor(`DayType`)]

raw[is.na(steps), "steps"] <- raw[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
int <- raw[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `DayType`)]

ggplot(int, aes(x = interval, y = steps, color=`DayType`)) + geom_line() + labs(title = "Comparison of Weekday vs. Weekend Average Daily Steps", x = "interval", y = "Count of Steps") + facet_wrap(~`DayType`, ncol = 1, nrow=2)
