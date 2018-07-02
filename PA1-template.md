---
output:
  word_document: default
  html_document: default
---

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

## Loading and preprocessing the data


```r
setwd("G:/Data Science/5. Reproducible Research/Week2")
data <- read.table(file = "activity.csv", header = TRUE, sep=",")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
totalstepsperday <- aggregate(steps~date, data, sum)
totalstepsperday
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```


2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
ggplot(totalstepsperday, aes(steps)) + geom_histogram(fill = 4, binwidth = 1000) + labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meansteps <- mean(totalstepsperday$steps, na.rm=TRUE)
mediansteps <- median(totalstepsperday$steps, na.rm=TRUE)
```
The mean steps are 1.0766189 &times; 10<sup>4</sup> and the median steps are 10765


## What is the average daily activity pattern?
1. Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averagestepsperday <- aggregate(steps~interval, data, mean)
ggplot(averagestepsperday, aes(interval, steps)) + geom_line(col = 4, lwd=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Average Steps per Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averagestepsperday[averagestepsperday$steps == max(averagestepsperday$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)


```r
missingvalues <- sum(is.na(data$steps))
```
The total number of missing values are 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Fill in all the misiing values with the mean of steps. 


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data2 <- data
data2$steps[is.na(data2$steps)] <- mean(data$steps, na.rm = TRUE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
averagestepsperday2 <- aggregate(steps ~ date, data2, sum, na.rm=TRUE)
par(mfrow = c(1, 2))
plot(averagestepsperday, type = "l", lwd = 5,lend = "square", main = "With NAs", col = 3)
abline(h = seq(0, 20000, 2500), lty = "dashed", col = 4)
plot(averagestepsperday2, type = "l", lwd = 5, lend = "square", main = "NAs filled", col = 3)
abline(h = seq(0, 20000, 2500), lty = "dashed", col = 4)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mean(data2$steps)
```

```
## [1] 37.3826
```

```r
median(data2$steps)
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$date <- as.Date(data$date, format = "%Y-%m-%d") 
data$dayname <- format(data$date, "%A")
data[grepl("Monday|Tuesday|Wednesday|Thrusday|Friday", data$dayname), "weekday_or_weekend"] <- "weekday"
data[grepl("Saturday|Sunday", data$dayname), "weekday_or_weekend"] <- "weekend"
data$`weekday_or_weekend` <- factor(data$`weekday_or_weekend`)
```

2. Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgsteps <- aggregate(steps ~ interval + weekday_or_weekend, data, mean, na.rm = TRUE)
ggplot(avgsteps, aes(interval, steps)) + geom_line(aes(color = weekday_or_weekend), lwd = 1) + facet_wrap(~weekday_or_weekend) + labs(title = "Average Daily Steps by weektype",y= "No. of Steps", x = "Interval")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
