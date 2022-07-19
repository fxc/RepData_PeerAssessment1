---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Load library

```r
library("lubridate")
```

```
## 
## 载入程辑包：'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library("dplyr")
```

```
## 
## 载入程辑包：'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library("ggplot2")
```
## Loading and preprocessing the data
load data from csf file `activity.csv`

```r
act_data_ori <- read.csv("activity.csv")
```
select data that `steps` is not NA

```r
act_data <- act_data_ori[which(!is.na(act_data_ori$steps)), ]
act_data$date <- ymd(act_data$date)
```

## What is mean total number of steps taken per day?
group by `date`, summarize `steps` by function `sum`, and plot histogram by total steps each day


```r
total_steps_per_day <- group_by(act_data, date) %>% summarize(total_steps = sum(steps))
g <- ggplot(total_steps_per_day, aes(total_steps))
g + geom_histogram(fill = "orange", alpha = 0.6, bins = 30) + labs(x = "Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Mean of total steps per day is:

```r
t_mean <- mean(total_steps_per_day$total_steps)
print(paste("Mean of total steps per day:", as.character(t_mean)))
```

```
## [1] "Mean of total steps per day: 10766.1886792453"
```

Median of total steps per day is:

```r
t_media <- median(total_steps_per_day$total_steps)
print(paste("Median of total steps per day:", as.character(t_media)))
```

```
## [1] "Median of total steps per day: 10765"
```

## What is the average daily activity pattern?
group by `interval` and summarize `steps` by function `mean`, make a time series plot


```r
avg_daily_act_pattern <- group_by(act_data, interval) %>% summarize(avg_act_pattern = mean(steps))
avg_daily_act_pattern$date_time <- ymd_hm(paste('2012-10-01', paste(as.character(avg_daily_act_pattern$interval %/% 100), as.character(avg_daily_act_pattern$interval %% 100), sep = ":"), sep = " "))
g <- ggplot(avg_daily_act_pattern, aes(date_time, avg_act_pattern))
g + geom_line(col = "orange") + labs(y = "Average Steps") + scale_x_datetime(date_labels = '%H:%M', date_breaks = '2 hours')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The 5-minute interval contains maximum number of average steps across all the days is:

```r
max_interval <- avg_daily_act_pattern$interval[which.max(avg_daily_act_pattern$avg_act_pattern)]
max_interval
```

```
## [1] 835
```

## Imputing missing values

```r
na_rows <- nrow(act_data_ori[which(is.na(act_data_ori$steps)), ])
print(paste("Total number of missing values is:", na_rows))
```

```
## [1] "Total number of missing values is: 2304"
```

create a new dataset, fill missing `steps` with the mean for 5-minute interval

```r
act_data_new <- act_data_ori
act_data_new$steps <- ifelse(is.na(act_data_new$steps), avg_daily_act_pattern$avg_act_pattern[act_data_new$interval %/% 100 + (act_data_new$interval %% 100) %/% 5], act_data_new$steps)
```
with new dataset, group by `act_data_new`, summarize `steps` by function `sum`, and plot histogram by total steps each day


```r
total_steps_per_day_new <- group_by(act_data_new, date) %>% summarize(total_steps = sum(steps))
g <- ggplot(total_steps_per_day_new, aes(total_steps))
g + geom_histogram(fill = "blue", alpha = 0.7) + labs(x = "Total Steps Per Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean of total steps per day in new dataset is:

```r
t_mean_new <- mean(total_steps_per_day_new$total_steps)
t_mean_new
```

```
## [1] 9371.365
```

Median of total steps per day in new dataset is:

```r
t_median_new <- median(total_steps_per_day_new$total_steps)
t_median_new
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?

create a new variable `weekend`, 0: weekday, 1: weekend

```r
act_data_new$weekend <- ifelse(wday(act_data_new$date) %in% c(1, 7), 1, 0)
```
group by `weekend` and `interval`, summarize `steps` with function mean, plot the differences in activity patterns between weekdays and weekends

```r
avg_steps_data <- group_by(act_data_new, weekend, interval) %>% summarize(avg_steps = mean(steps), .groups = "keep")
week_factor <- factor(avg_steps_data$weekend, labels = c('Weekday', 'Weekend'))
avg_steps_data$date_time <- ymd_hm(paste('2012-10-01', paste(as.character(avg_steps_data$interval %/% 100), as.character(avg_steps_data$interval %% 100), sep = ":"), sep = " "))
g <- ggplot(avg_steps_data, aes(date_time, avg_steps, color = week_factor))
g + geom_line() + facet_grid(weekend ~ .) + labs(x = "Interval", y = "Average Steps", title = "The differences in activity patterns between weekdays and weekends") + scale_x_datetime(date_breaks = "2 hours", date_labels = '%H:%M')
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

