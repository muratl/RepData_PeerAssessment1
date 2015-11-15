# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
- Load the data (i.e. read.csv())


```r
unzip(zipfile="activity.zip")
```

- Process/transform the data (if necessary) into a format suitable for your analysis


```r
activityData <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
- Calculate the total number of steps taken per day


```r
stepsPerDay <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
```

- Plotting bar charts


```r
library(ggplot2)

qplot(stepsPerDay, binwidth=1000, xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/steps_per_day_2-1.png) 

- Calculate mean


```r
mean(stepsPerDay, na.rm=TRUE)
```

```
## [1] 9354.23
```


- Calculate median


```r
median(stepsPerDay, na.rm=TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageActivity <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageActivity, aes(x=interval, y=steps)) +
      geom_line() +
      xlab("5-minute interval") +
      ylab("Average number of steps")
```

![](PA1_template_files/figure-html/average_daily_pattern_1-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageActivity[which.max(averageActivity$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missingActivity <- is.na(activityData$steps)

# How many missing

table(missingActivity)
```

```
## missingActivity
## FALSE  TRUE 
## 15264  2304
```

- All of the missing values are filled in with mean value for that 5-minute
interval.


```r
# Function: Replace each missing value with the mean value of its 5-minute interval
replace_missing_value <- function(steps, interval) {
    replaced <- NA
    
    if (!is.na(steps))
        replaced <- c(steps)
    else
        replaced <- (averageActivity[averageActivity$interval==interval, "steps"])

    return(replaced)
}
```


- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filledActivityData <- activityData

filledActivityData$steps <- mapply(replace_missing_value, filledActivityData$steps, filledActivityData$interval)
```


- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDay2 <- tapply(filledActivityData$steps, filledActivityData$date, FUN=sum, na.rm=TRUE)

qplot(stepsPerDay2, binwidth=1000, xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/missing_value_4-1.png) 

```r
mean(stepsPerDay2)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay2)
```

```
## [1] 10766.19
```

-> Mean and median values are higher after imputing missing data.
-> In the first calculation, NA were replaced by 0, lowering mean and median.


## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# Function to determine if day is a weekday or not

weekOrWeekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("Weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("Weekend")
    else
        # stop("invalid date")
        return ("None")
}

filledActivityData$date <- as.Date(filledActivityData$date)

filledActivityData$day <- sapply(filledActivityData$date, FUN=weekOrWeekend)
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averageActivity2 <- aggregate(steps ~ interval + day, data=filledActivityData, mean)

ggplot(averageActivity2, aes(interval, steps)) + 
       geom_line() + 
       facet_grid(day ~ .) +
       xlab("5-minute interval") + 
       ylab("Number of steps")
```

![](PA1_template_files/figure-html/activity_patterns_2-1.png) 
