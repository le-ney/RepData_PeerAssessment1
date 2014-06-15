Reproducible Research: Peer Assessment 1
========================================================

The data for this assignment can be downloaded from [Activity monitoring data]("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip") or from my GitHub reposetory.

## Loading and preprocessing the data

The following code loads the data to dataframe

```r
fname <- "activity.zip"
fname <- unz(fname,"activity.csv")
dat <- read.csv(fname, header=TRUE)
dat$date <- as.Date(dat$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

#### 1. Make a histogram of the total number of steps taken each day
Calculating the total number of steps by day. Please note the missing values in the dataset has been excluded in all the calculations 

```r
sumdat<- aggregate(dat$steps, by = list(dat$date), sum, na.rm=TRUE)
colnames(sumdat) <- c("Day","Steps")
with(sumdat, hist(Steps, col="darkorchid2",main="Histogram of the total number of steps taken each day", xlab="Number of Steps", ylab="Count"))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
#### 2. The mean and median total number of steps taken per day 
The mean for the total steps per day is aclculated as follows:


```r
mean(sumdat$Steps)
```

```
## [1] 9354
```
The median for the total steps per day is aclculated as follows:


```r
median(sumdat$Steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

#### 1. Histogram of the total number of steps taken each day

```r
sumdat <- aggregate(dat$steps, by = list(dat$interval), mean, na.rm=TRUE)
colnames(sumdat) <- c("Interval","Steps")
plot(sumdat$Interval,sumdat$Steps, xlab="Interval", ylab="Total number of steps", type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
sumdat[which.max(sumdat$Steps),1]
```

```
## [1] 835
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```
#### 2. Devise a strategy for filling in all of the missing values in the dataset.

```r
misVals <- dat[is.na(dat$steps),c(2,3)]
newdat <- na.omit(dat)
colnames(sumdat) <- c("interval","steps")
misVals <- merge(x=misVals, y=sumdat, by="interval", all.x=TRUE)
misVals$steps <- round(misVals$steps)
```
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdat <- rbind (newdat, misVals)
```
#### 4. Make a histogram of the total number of steps taken each day

```r
sumdat <- aggregate(newdat$steps, by = list(dat$date), sum, na.rm=TRUE)
colnames(sumdat) <- c("Day","steps")
with(sumdat, hist(steps, col="green3",main="Histogram of the total number of steps taken each day", xlab="Number of Steps", ylab="Count"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

The mean total number of steps taken per day:

```r
mean(sumdat$steps)
```

```
## [1] 10766
```

The median total number of steps taken per day:

```r
median(sumdat$steps)
```

```
## [1] 11015
```
## Are there differences in activity patterns between weekdays and weekends?

#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newdat$daytype <- weekdays(newdat$date)
newdat$daytype[newdat$daytype=="Saturday" | newdat$daytype=="Sunday"] <- "weekend"
newdat$daytype[newdat$daytype !="weekend"] <- "weekday"
newdat$daytype <- as.factor(newdat$daytype)
```
#### 2. Make a panel plot containing a time series plot 

```r
sumdat <- aggregate(newdat$steps, by = list(newdat$daytype, newdat$interval), mean, na.rm=TRUE)
colnames(sumdat) <- c("daytype", "interval", "mean")
library(lattice)
xyplot(sumdat$mean ~ sumdat$interval | sumdat$daytype, layout = c(1, 2), type = "l", lwd=2 , xlab= "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 