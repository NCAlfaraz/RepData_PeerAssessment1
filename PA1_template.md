---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
defaultW <- getOption("warn") 

options(warn = -1) 
```

## Loading and preprocessing the data
1. Code for reading in the dataset and/or processing the data


```r
library(readr)
activity <- read_csv("activity.csv")
```

## What is mean total number of steps taken per day?
2. Histogram of the total number of steps taken each day


```r
library(tidyr)
library(tidyverse)
library(tidytext)
steps_per_day <- activity %>%
  filter(!is.na(steps))%>%
  group_by(date) %>%
  summarise (stepsPerDay = sum(steps))

hist(steps_per_day$stepsPerDay, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The mean and median of the total number of steps taken per day are 10766 and 10765 respectively.

```r
summary(steps_per_day)
```

```
##       date             stepsPerDay   
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

## What is the average daily activity pattern?

1. Filter all the NA values across the dataset

```r
avgDailyAct <- activity %>%
  filter(!is.na(steps))%>%
  group_by(interval)  %>% summarise(avgSteps = mean(steps))
head(avgDailyAct)
```

```
## # A tibble: 6 × 2
##   interval avgSteps
##      <dbl>    <dbl>
## 1        0   1.72  
## 2        5   0.340 
## 3       10   0.132 
## 4       15   0.151 
## 5       20   0.0755
## 6       25   2.09
```
2. Make the time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2);

ggplot(avgDailyAct, aes(interval, avgSteps))+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Imputing missing values

1. Find which interval has the maximum number of steps

```r
#The interval 835 has the maximum number of steps which is 206
whichmax <- which.max(avgDailyAct$avgSteps)
print(avgDailyAct[whichmax,])
```

```
## # A tibble: 1 × 2
##   interval avgSteps
##      <dbl>    <dbl>
## 1      835     206.
```
2. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).  

```r
#Find and calculate  the NA value across the dataset
print(sum(is.na(activity)))
```

```
## [1] 2304
```
3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#Fill the missing value with the mean value for that 5-minute interval
df_without_na <- aggregate(steps ~ interval, FUN=mean, data=activity)
head(df_without_na)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
4. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Merge the original dataset and the modified dataset(without NA)
df.merge <- merge(x = activity, y=df_without_na, by="interval")
## here the original steps column name is the same as steps x, while the steps.y column is the mean value for each interval
df.merge$steps.x <- ifelse(is.na(df.merge$steps.x), df.merge$steps.y, df.merge$steps.x)
colnames(df.merge) <- c("interval", "steps", "date", "meanPerInterval")
df <- df.merge[c("steps", "date", "interval")]
```

5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
df.final <- df %>%
  group_by(date)  %>% summarise(stepsPerDay = sum(steps)) 
#construct the histogram 
hist(df.final$stepsPerDay, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#print the mean and median value
print(mean(df.final$stepsPerDay))
```

```
## [1] 10766.19
```

```r
print(factor(median(df.final$stepsPerDay)))
```

```
## [1] 10766.1886792453
## Levels: 10766.1886792453
```
Conclusion: By replacing missing values, the median value shifted closer or approximately equal to mean value.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.

```r
weekdayAndWeekend <- df %>%
  select(steps, date, interval)%>%
  mutate(category = as.factor(ifelse(weekdays(as.Date(date)) %in% c("Saturday", "Sunday"), "weekend", "weekdays")))  
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
df.weekdayWeekend <- weekdayAndWeekend %>%
  group_by(category, interval) %>% summarise(avg = mean(steps))
```

```
## `summarise()` has grouped output by 'category'. You can override using the
## `.groups` argument.
```

```r
xyplot(avg ~ interval | category, df.weekdayWeekend
     , type = "l"
     , xlab = "Interval"
     , ylab = "Number of steps"
     , main = "Average number of steps taken, across all weekday and weekend"
     , layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
