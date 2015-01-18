# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
StepsTotal <- aggregate(steps ~ date, data, sum, na.rm=TRUE )
barplot(StepsTotal$steps,names.arg=StepsTotal$date,main ="Histogram of the total number of steps taken each day", xlab="date",ylab="steps",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(StepsTotal$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
median(StepsTotal$steps,na.rm=TRUE)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepsInterval <- aggregate(steps ~ interval, data, mean,na.rm=TRUE)
plot(StepsInterval,type="l",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
StepsInterval$interval[which.max(StepsInterval$steps)]
```

```
## [1] 835
```
## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data <- merge(data, StepsInterval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(data$steps)
data$steps[nas] <- data$steps.y[nas]
data <- data[, c(1:3)]
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
StepDate <- aggregate(steps ~ date, data, sum)
barplot(StepDate$steps, names.arg = StepDate$date, main ="Histogram of the total number of steps taken each day",xlab = "date", ylab = "steps",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


```r
mean(StepDate$steps)
```

```
## [1] 10766.19
```


```r
median(StepDate$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the **weekdays()** function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
data$daytype <- as.factor(sapply(data$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


