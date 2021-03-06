#Loading and preprocessing the data

#(1)Load the data (i.e. read.csv())
dat<-read.csv("activity.csv",stringsAsFactors = FALSE)

#(2)Process/transform the data (if necessary) into a format suitable for your analysis
dat2<-transform(dat, date = as.Date(date, "%Y-%m-%d"))


#What is mean total number of steps taken per day?
dat2<-subset(dat2, complete.cases(dat2))

#(1)Calculate the total number of steps taken per day
stepsperday<-with(dat2,tapply(steps,date,sum))
print(paste("Total number of steps per day is ",stepsperday))

#(2)Make a histogram of the total number of steps taken each day
hist(stepsperday,col = "red",xlab="Steps Per Day",main = "Histogram Of Steps Taken Per Day Vs Frequency")

#(3)Calculate and report the mean and median of the total number of steps taken per day
meansteps<-mean(stepsperday)
mediansteps<-median(stepsperday)
print(paste("The mean and the median of the total number of steps taken per day are",
            meansteps," and ",mediansteps," respectively"))


#What is the average daily activity pattern

#(1)Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#the average number of steps taken, averaged across all days (y-axis)
avesteps <- ddply(dat2, .(interval), .fun = function(x){ return(mean(x$steps)) })
colnames(avesteps)[2] <- "steps"
plot(avesteps$interval,avesteps$steps, type="l", xlab="Interval", ylab="Average Number of Steps")

#(2)Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxinterval <- avesteps$interval[which.max(avesteps$steps)]


#Imputing missing values
#(1)Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totna<-nrow(dat)-nrow(dat2)
print(paste("Total number of NA's is ",totna))

#(2)Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.
datna<-subset(dat,is.na(dat$steps))
for(i in 1:nrow(datna)){
     datna[i,1]<-subset(avesteps,interval==datna[i,3])[,2]
}

#(3)Create a new dataset that is equal to the original dataset but with the missing data filled in.
newdat<-rbind(dat2,datna)

#(4)Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
#total number of steps taken per day. Do these values differ from the estimates from the first part of the 
#assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
newstepsperday<-with(newdat,tapply(steps,date,sum))
hist(newstepsperday,col = "red",xlab="Steps Per Day",main = "Histogram Of Steps Taken Per Day Vs Frequency")

#Are there differences in activity patterns between weekdays and weekends?
#(1)Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
newdat$week<-weekdays(newdat[,2])
newdat$week<-factor(ifelse(newdat$week %in% c("Saturday","Sunday"),"weekend","weekday"))

#(2)Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look like using 
#simulated data.
final <-with(newdat, aggregate(steps, list(interval, week), FUN=mean))
names(final) <- c("interval","week","steps")
library(lattice)
xyplot(final$steps ~ final$interval | final$week, type='l', xlab="Interval", 
       ylab="Number of Steps",layout = c(1,2))
