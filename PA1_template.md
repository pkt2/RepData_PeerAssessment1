---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Setting globe options
```{r , echo = FALSE}
opts_chunk$set(echo = TRUE, results = "asis)
```

## Loading and preprocessing the data
``{r}
data <- read.table(file.choose(), header = TRUE, sep = ",")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```{r total_steps, echo = TRUE, results = "hide}
totalstepsperday <- aggregate(steps~date, data, sum)
```
Total number of steps per day are `r totalstepsperday`

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 

```{r}
ggplot(totalstepsperday, aes(steps)) + geom_histogram(fill = 4, binwidth = 1000) + labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
meansteps <- mean(totalstepsperday$steps, na.rm=TRUE)
mediansteps <- median(totalstepsperday$steps, na.rm=TRUE)
```
The mean steps are `r meansteps` and the median steps are `r mediansteps`


## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
averagestepsperday <- aggregate(steps~interval, data, mean)
ggplot(averagestepsperday, aes(interval, steps)) + geom_line(col = 4, lwd=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Average Steps per Day")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
averagestepsperday[averagestepsperday$steps == max(averagestepsperday$steps),]
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```{r}
missingvalues <- sum(is.na(data$steps))
```
The total number of missing values are `r missingvalues`

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Fill in all the misiing values with the mean of steps. 


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
data2 <- data
data2$steps[is.na(data2$steps)] <- mean(data$steps, na.rm = TRUE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
averagestepsperday2 <- aggregate(steps ~ date, data2, sum, na.rm=TRUE)
par(mfrow = c(1, 2))
plot(averagestepsperday, type = "l", lwd = 5,lend = "square", main = "With NAs", col = 3)
abline(h = seq(0, 20000, 2500), lty = "dashed", col = 4)
plot(averagestepsperday2, type = "l", lwd = 5, lend = "square", main = "NAs filled", col = 3)
abline(h = seq(0, 20000, 2500), lty = "dashed", col = 4)

mean(data2$steps)
median(data2$steps)
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
data$date <- as.Date(data$date, format = "%Y-%m-%d") 
data$dayname <- format(data$date, "%A")
data[grepl("Monday|Tuesday|Wednesday|Thrusday|Friday", data$dayname), "weekday_or_weekend"] <- "weekday"
data[grepl("Saturday|Sunday", data$dayname), "weekday_or_weekend"] <- "weekend"
data$`weekday_or_weekend` <- factor(data$`weekday_or_weekend`)
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
avgsteps <- aggregate(steps ~ interval + weekday_or_weekend, data, mean, na.rm = TRUE)
ggplot(avgsteps, aes(interval, steps)) + geom_line(aes(color = weekday_or_weekend), lwd = 1) + facet_wrap(~weekday_or_weekend) + labs(title = "Average Daily Steps by weektype",y= "No. of Steps", x = "Interval")
```

