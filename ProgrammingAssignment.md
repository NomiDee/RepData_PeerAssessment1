Introduction
============

This is Programming Assignment 1 for the coursera course Reproducible Research.

Assignment
==========

``` r
library(ggplot2)
monitoringData<-read.csv("activity.csv")  
```

### What is the mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
1. Make a histogram of the total number of steps taken each day

``` r
stepsPerDay<-aggregate(steps~date,monitoringData,sum)
barplot(stepsPerDay$steps, names.arg=stepsPerDay$date, main="Number of Steps per Day", xlab="Date", ylab="Number of Steps") 
```

![plot of chunk unnamed-chunk-3](ProgrammingAssignment_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median total number of steps taken per day

``` r
round(mean(stepsPerDay$steps))
```

    ## [1] 10766

``` r
median(stepsPerDay$steps)
```

    ## [1] 10765

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
stepsPerInterval<-aggregate(steps~interval, monitoringData, mean)
plot(stepsPerInterval$interval, stepsPerInterval$steps, type="l", col="blue", xlab="Time Interval", ylab="Average Number of Steps", main = "Average Number of Steps per Time Interval")
```

![plot of chunk unnamed-chunk-5](ProgrammingAssignment_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxSteps<-max(stepsPerInterval$steps)
maxSteps<-round(maxSteps)
maxStepInterval<-stepsPerInterval$interval[which.max(stepsPerInterval$steps)]
```

The maximum number of average steps is 206 at time interval 835.

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NAs`)

``` r
missingData<-sum(is.na(monitoringData))
```

The number of missing values in the data set is 2304.

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

This code will use the mean for 5-minute intervals to replace `NAs`.

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
#Load library to enable left_join 
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#Find missing data
missingSteps<-is.na(monitoringData$steps)

#Create new data set from original dataset, adding a column of average steps per interval, using by interval.
monitoringData2<-left_join(monitoringData, stepsPerInterval, by = "interval")

#Replace missing step data with average steps for the 5-minute interval.
monitoringData2[missingSteps, ]$steps.x <- monitoringData2[missingSteps,]$steps.y

#Keep only necessary columns in the dataset.
monitoringData2<-select(monitoringData2, steps=steps.x, date, interval)    
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
stepsPerDay<-aggregate(steps~date,monitoringData2,sum)
g<-ggplot(stepsPerDay, aes(x=date, y=steps)) + geom_bar(stat="identity") + labs(title="Number of Steps Taken per Day", x = "Date", y = "Number of Steps") + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust=0.5))

avg<-mean(stepsPerDay$steps)
med<-median(stepsPerDay$steps)

#Add mean line
g + geom_hline(yintercept=avg, col="blue") + geom_text(aes(0, avg, label = paste("Mean = ", round(avg)), vjust = -1), size = 3)
```

![plot of chunk unnamed-chunk-9](ProgrammingAssignment_files/figure-markdown_github/unnamed-chunk-9-1.png
``` r
avg<-format(avg, scientifc=FALSE)
med<-format(med, scientifc=FALSE)
```

-   The mean (imputed) steps per day is: 10766.19.
-   The median (imputed) steps per day is: 10766.19.
-   Imputing the missing data did not change the mean and slightly increased the median to equal the mean.

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
monitoringData2$day<-weekdays(as.Date(monitoringData2$date))
monitoringData2$wkd<-ifelse(grepl("S(at|un)", monitoringData2$day),"Weekend","Weekday")
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
stepsPerInterval2<-aggregate(steps~interval+wkd, monitoringData2, mean)

p<-ggplot(stepsPerInterval2, aes(x=interval, y=steps)) + geom_line() + labs(title="Number of Steps taken on Weekdays versus Weekends", x = "5-minute Interval", y = "Number of Steps") + theme_bw() + facet_grid(wkd~.)
print(p)
```

![plot of chunk unnamed-chunk-11](ProgrammingAssignment_files/figure-markdown_github/unnamed-chunk-11-1.png)
