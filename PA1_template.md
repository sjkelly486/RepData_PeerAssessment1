---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
In this section, I am reading setting my working directory, clearing environmental variables, loading ggplot2 package, and then reading in the activity data.


```r
setwd("~/Career/DataScience/Courses/JohnsHopkins_DataScience/Courses/5_Reproducible_Research/RepData_PeerAssessment1/RepData_PeerAssessment1")
rm(list = setdiff(ls(), lsf.str())) #remove all variables
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.2
```

```r
activity <- read.csv("activity.csv",stringsAsFactors = FALSE)
```



## What is mean total number of steps taken per day?
Next, I calculate the total number of steps taken per day, and plot it in a histogram.


```r
stp_day <- xtabs(steps~date,activity)
hist(stp_day,xlab="Number of Steps Per Day", main="Histogram of Number of Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Then, I calculate the mean and median of the total number of steps taken per day

```r
mean_stp_day <- mean(stp_day)
median_stp_day <- median(stp_day)
```
The mean number of steps per day is 1.0766189\times 10^{4}
The median number of steps per day is 10765



## What is the average daily activity pattern?
In order to determine the pattern across all days, I average the data at each 5 minute interval across all the days in the data. The plot below shows the average number of steps taken for each interval.


```r
ndays <- length(unique(activity$date))
stp_interval <- data.frame(xtabs(steps~interval,activity)/ndays)
stp_interval$interval_int <- as.integer(levels(stp_interval$interval))[stp_interval$interval]

g<- ggplot(stp_interval, aes(x=interval_int,y=Freq))
g<- g + geom_line()
g<- g + labs(x="5-Minute Interval",y="Average Number of Steps")
g<- g + labs(title="Average Number of Steps for Each 5-Minute Interval over 2 Months")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
interval_max_avg_stp <- stp_interval$interval[which.max(stp_interval$Freq)]
```

The 5 minute interval starting at 835 on average contains the most number of steps.



## Imputing missing values

```r
miss_data <- is.na(activity$steps)
num_miss <- sum(miss_data)
```

There are a 2304 missing values in the data set.In order to determine how these missing values are affecting results, I filled in any missing values with the average value across all days for that interval (plotted previously).

```r
#This line will find the index in stp_interval that matches each row in activity$interval
mtch_interval <- match(activity$interval,stp_interval$interval)

#Use mtch_interval create new variable activity2, which repalces any missing data in activity with with average steps for that interval
activity2 <- data.frame(activity)
activity2$steps[miss_data] <- stp_interval$Freq[mtch_interval[miss_data]]
```
In the code above, new variable activity2 fills in all the missing data in the original activity data. The following histogram shows the total number of steps taken each day with the imputed data in activity2.


```r
stp_day2 <- xtabs(steps~date,activity2)
hist(stp_day2,xlab="Number of Steps Per Day", main="Histogram of Number of Steps Taken Per Day from Imputed Data")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean_stp_day2 <- mean(stp_day2)
median_stp_day2 <- median(stp_day2)
```

With the imputed data, the mean number of steps per day is 1.0581014\times 10^{4}, and the median number of steps per day is 1.0395\times 10^{4}. By comparing with the mean and median values computing on the original data, we can see that imputing the data did not change the mean or median by very much. Additionaly, the qualitative shape of the histograms of total number of steps taken per day did not change.



## Are there differences in activity patterns between weekdays and weekends?
The imputed data will be used in this section to determine if there are differences in the patterns of the subject between weekdays and weekends.

To accomplish this, I first use the as.Date() and weekdays() function to determine which day of the week corresponds to the dates in the data.


```r
activity2$date<- as.Date(activity2$date, "%Y-%m-%d")
activity2$day<- weekdays(activity2$date)
```

Then, I create a new factor variable called daytype which specifies if the day is a Weekday or Weekend.

```r
activity2$daytype <- as.factor(ifelse(activity2$day=="Saturday" | activity2$day=="Sunday", "Weekend", "Weekday"))
```

Next, the tapply function is used to find the mean number of steps across two of variables in activity2 (interval & steps)

```r
AvgStp2 <- with(activity2, tapply(steps,list(interval,daytype),mean))
```

Then, I re-collect the output from tapply into a new dataframe called activity2Type

```r
intervals <- dimnames(AvgStp2)[[1]]
daytypes <- dimnames(AvgStp2)[[2]]
num_interval <- length(intervals)

activity2Type<-data.frame(AvgStp=c(AvgStp2[,1],AvgStp2[,2]),
                 interval=as.integer(rep(intervals,2)),
                 daytype=c(rep(daytypes[1],num_interval),rep(daytypes[2],num_interval)))
```

Finally, I generate a time-series plot showing the different average activity for weekdays and weekends

```r
g<- ggplot(activity2Type, aes(x=interval,y=AvgStp))
g<- g + geom_line()
g<- g + facet_wrap(daytype~.,nrow=2,ncol=1)
g<- g + labs(x="5-Minute Interval",y="Average Number of Steps")
g<- g + labs(title="Average Number of Steps for Each 5-Minute Interval over 2 Months")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
