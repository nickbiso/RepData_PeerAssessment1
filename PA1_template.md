Reproducible Research Week 1 Assignment 
----  

###Instructions:

####Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

**Dataset:** Activity monitoring data [52K]
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
  
---------------------------------------------------------------------------------------------------------------

## Loading and preprocessing the data


### Show any code that is needed to:




1. Load the data (i.e. read.csv()) 

```r
dat<-read.csv("activity.csv",stringsAsFactors = FALSE)
head(dat)
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

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
dat2<-transform(dat, date = as.Date(date, "%Y-%m-%d"))
head(dat)
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


###What is mean total number of steps taken per day?

```r
dat2<-subset(dat2, complete.cases(dat2))
```
1. Calculate the total number of steps taken per day

```r
stepsperday<-with(dat2,tapply(steps,date,sum))
```

2. Make a histogram of the total number of steps taken each day

```r
hist(stepsperday,col = "red",xlab="Steps Per Day",main = "Histogram Of Steps Taken Per Day Vs Frequency")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
meansteps<-mean(stepsperday)
mediansteps<-median(stepsperday)
```

```
## [1] "The mean and the median of the total number of steps taken per day are 10766.1886792453  and  10765  respectively"
```

###What is the average daily activity pattern

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
the average number of steps taken, averaged across all days (y-axis)  

```r
library(plyr)
avesteps <- ddply(dat2, .(interval), .fun = function(x){ return(mean(x$steps)) })
colnames(avesteps)[2] <- "steps"
plot(avesteps$interval,avesteps$steps, type="l", xlab="Interval", ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxinterval <- avesteps$interval[which.max(avesteps$steps)]
print(paste("The 5 minute interval that contains the maximum number of steps is ", maxinterval))
```

```
## [1] "The 5 minute interval that contains the maximum number of steps is  835"
```

###Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totna<-nrow(dat)-nrow(dat2)
```

```
## [1] "Total number of NA's is  2304"
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
or the mean for that 5-minute interval, etc.

```r
datna<-subset(dat,is.na(dat$steps))
for(i in 1:nrow(datna)){
     datna[i,1]<-subset(avesteps,interval==datna[i,3])[,2]
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdat<-rbind(dat2,datna)
head(newdat)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
total number of steps taken per day. Do these values differ from the estimates from the first part of the 
assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
newstepsperday<-with(newdat,tapply(steps,date,sum))
hist(newstepsperday,col = "red",xlab="Steps Per Day",main = "Histogram Of Steps Taken Per Day Vs Frequency")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

```
## [1] "The new mean number of steps is  10766.1886792453"
```

```
## [1] "The new median number of steps is  10766.1886792453"
```


###Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
indicating whether a given date is a weekday or weekend day.

```r
newdat$week<-weekdays(newdat[,2])
newdat$week<-factor(ifelse(newdat$week %in% c("Saturday","Sunday"),"weekend","weekday"))
head(newdat)
```

```
##     steps       date interval    week
## 289     0 2012-10-02        0 weekday
## 290     0 2012-10-02        5 weekday
## 291     0 2012-10-02       10 weekday
## 292     0 2012-10-02       15 weekday
## 293     0 2012-10-02       20 weekday
## 294     0 2012-10-02       25 weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
See the README file in the GitHub repository to see an example of what this plot should look like using 
simulated data.


```r
final <-with(newdat, aggregate(steps, list(interval, week), FUN=mean))
names(final) <- c("interval","week","steps")
library(lattice)
xyplot(final$steps ~ final$interval | final$week, type='l', xlab="Interval", 
       ylab="Number of Steps",layout = c(1,2))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

During the weekend, there is a more consistent pattern of personal movement while during the weekday there is a 
spike on the first half of the day. They clearly have different patterns of movement which can be caused by the 
different activities done during the weekday like office work(9-5) and hobbies done during the different times 
during the weekend.




