# Activity monitoring data analysis
Yuan Dong  
8/21/2017  



## Read me
Here I analysed data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data can be downloaded from the course web site:
[link](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:
steps: Number of steps taking in a 5-minute interval (missing values are coded as ????????)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file.

## Prosessing the data
### 1 Read in the data and change date into correct formmat

```r
data<-read.csv("RRweek2activity.csv")
data$dateformmat<-as.Date(as.character(data$date, "%Y-%m-%d"))
```

### 2 Histogram of the total number of steps taken each day
Calculate the total number of steps taken per day.
Make a histogram of the total number of steps taken each day

```r
totalsteps<-aggregate(steps~dateformmat,data=data, sum)
library(ggplot2)
qplot(steps, data=totalsteps, main="total number of steps taken each day", xlab="total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 3 Calculate and report the mean and median of the total number of steps taken per day
Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalsteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalsteps$steps)
```

```
## [1] 10765
```


### 4 Time series plot of the average number of steps taken
Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalsteps<-aggregate(steps~interval, data=data, mean)
qplot(interval, steps, data=intervalsteps, geom="line", main="Time series of average steps", ylab="average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 5 The 5-minute interval that, on average, contains the maximum number of steps

```r
maxinterval<-intervalsteps[which.max(intervalsteps$steps),]
print(maxinterval$interval)
```

```
## [1] 835
```

### 6 Code to describe and show a strategy for imputing missing data
Calculate and report the total number of missing values in the dataset

```r
nrow(data[!complete.cases(data),])
```

```
## [1] 2304
```

impute missing values with mean for that 5-minute interval

```r
library(data.table)
library(Hmisc)
imputedata<-data.table(data)
imputedata[, imput_steps := impute(steps, mean), by=interval]
```

### 7 Histogram of the total number of steps taken each day after missing values are imputed

```r
imputetotalsteps<-aggregate(imput_steps~dateformmat, data=imputedata, sum)
qplot(imput_steps, data = imputetotalsteps,main="total number of steps taken each day", xlab="total steps(imputed missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
Add new virable indicating weekday or weekend

```r
imputedata$weekdays<-weekdays(imputedata$dateformmat)
fun<-function(imputedata){
  weekday_weekend<-c()
  for (i in 1:nrow(imputedata))
    if (imputedata[i,]$weekdays=="Saturday" | imputedata[i,]$weekdays=="Sunday"){
      weekday_weekend[i]<-"weekend"
    } 
    else{weekday_weekend[i]<-"weekday"}
    imputedata= cbind(imputedata, weekday_weekend)
}
imputedata2<-fun(imputedata)
```
Make the plot

```r
week_interval_steps<-aggregate(imput_steps~interval+weekday_weekend, data=imputedata2, mean)
qplot(interval,imput_steps, data= week_interval_steps, geom="line", facets = .~weekday_weekend,
      main="Time series of average steps classfied by weekday/weekend", ylab="average steps(imputed missing values)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
