# PA1_template
JGriffin  
April 9, 2016  

### **Loading and preprocessing the data**

Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.2
```

```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activity <- read.csv("./activity.csv", sep = ",", colClass=c('integer', 'Date', 'integer'))
head(activity)
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

### **What is mean total number of steps taken per day?**

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
total_steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE,  simplfy=TRUE)
head(total_steps)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          1        127      11353      12117      13295      15421
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps, breaks =20, xlab="Interval",ylab= "Steps", main ="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(total_steps, na.rm = TRUE)
```

```
## [1] 9355.23
```

```r
median(total_steps, na.rm = TRUE)
```

```
## [1] 10396
```

### **What is the average daily activity pattern?**

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.2
```

```r
steps_interval <- aggregate (steps ~ interval, activity, mean)
xyplot(steps ~ interval, data = steps_interval, type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_interval[which.max(steps_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
### **Imputing missing values**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Will use the mean for the 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### **Are there differences in activity patterns between weekdays and weekends?**

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dayType <- function(dates){
f <- function(date) {
  if (weekdays(date) %in% c("Saturday", "Sunday")){
    "weekend"
  } else{
    "weekday"
  }
}  
  sapply(dates,f)
}

activity$dayType <- as.factor(dayType(activity$date))
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ dayType : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```
  
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_interval <- aggregate (steps ~ interval + dayType, activity, mean)
xyplot(steps ~ interval | dayType, data = steps_interval, layout= c(1,2), type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


