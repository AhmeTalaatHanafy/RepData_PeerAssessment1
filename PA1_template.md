---
output: 
  html_document: 
    keep_md: yes
---
#Reproducible Research: Peer Assessment 1
==============================================

##Loading and preprocessing the data


```r
#Unzipping the archive
#unzip("activity.zip")
#Reading the data
raw_Data <- read.csv("activity.csv")

#Definig date data as date format
raw_Data$date <- as.Date(as.character(raw_Data$date))
```

**Summarizing the Data**

```r
summary(raw_Data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :0010-01-20   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:0010-07-12   1st Qu.: 588.8  
##  Median :  0.00   Median :0011-01-04   Median :1177.5  
##  Mean   : 37.38   Mean   :0011-01-04   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:0011-06-27   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :0011-12-20   Max.   :2355.0  
##  NA's   :2304     NA's   :10656
```


```r
str(raw_Data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "0010-01-20" "0010-01-20" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

##What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day


```r
#aggregate steps for each day
steps_Day <- aggregate(raw_Data$steps, by = list(raw_Data$date), sum)
#definig names
names(steps_Day) <- c("date", "total.Steps")
head(steps_Day, 20)
```

```
##          date total.Steps
## 1  0010-01-20          NA
## 2  0010-02-20         126
## 3  0010-03-20       11352
## 4  0010-04-20       12116
## 5  0010-05-20       13294
## 6  0010-06-20       15420
## 7  0010-07-20       11015
## 8  0010-08-20          NA
## 9  0010-09-20       12811
## 10 0010-10-20        9900
## 11 0010-11-20       10304
## 12 0010-12-20       17382
## 13 0011-01-20          NA
## 14 0011-02-20       10600
## 15 0011-03-20       10571
## 16 0011-04-20          NA
## 17 0011-05-20       10439
## 18 0011-06-20        8334
## 19 0011-07-20       12883
## 20 0011-08-20        3219
```

2.Make a histogram of the total number of steps taken each day


```r
#cleaning data 
steps_Day_NA <- is.na(steps_Day$total.Steps)
steps_Day_clean <- steps_Day[!steps_Day_NA, ]
#plot a histogram of cleaned data
library(ggplot2)
ggplot(steps_Day_clean, aes(x = steps_Day_clean$total.Steps)) + geom_histogram(fill = "orange", binwidth = 1000) +  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

**3.Calculate and report the mean and median of the total number of steps taken per day**


```r
mean(steps_Day_clean$total.Steps)
```

```
## [1] 10729.94
```


```r
median(steps_Day_clean$total.Steps)
```

```
## [1] 10890
```


##What is the average daily activity pattern?

1.Time series plot of the average number of steps taken


```r
#cleaning raw_Data
NA_Values <- is.na(raw_Data$steps)
raw_Data_clean <- raw_Data[!NA_Values, ]
#aggregate steps by intevals
mean_steps_interval <- aggregate(raw_Data_clean$steps, by = list(raw_Data_clean$interval), mean)

#defining new names
names(mean_steps_interval) <- c("interval", "steps")

#making a line plot
ggplot(mean_steps_interval, aes(x = interval, y = steps)) + geom_line(color = "orange") + labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/daily_Activity-1.png)<!-- -->

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
max_Interval <- mean_steps_interval[which.max(mean_steps_interval$steps), ]
max_Interval
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(NA_Values)
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset.
Strategy: Use mean interval steps from Mean Steps for that interval.


```r
#new dataset
raw_Data2 <- raw_Data

#define missing values
missing_values <- is.na(raw_Data2$steps)

#calculate mean values for clean data
mean_Values <- tapply(raw_Data_clean$steps, raw_Data_clean$interval, mean, na.rm=TRUE, simplify=TRUE)
#rounding the mean values
mean_Values <- round(mean_Values, 0)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#filling the missing values 
raw_Data2$steps[missing_values] <- mean_Values[as.character(raw_Data2$interval[missing_values])]
```



```r
#count missing values 
sum(is.na(raw_Data2$steps))
```

```
## [1] 0
```
**output = 0, means that all missing data have fullfilled**


```r
head(raw_Data2, 20)
```

```
##    steps       date interval
## 1      2 0010-01-20        0
## 2      0 0010-01-20        5
## 3      0 0010-01-20       10
## 4      0 0010-01-20       15
## 5      0 0010-01-20       20
## 6      2 0010-01-20       25
## 7      1 0010-01-20       30
## 8      1 0010-01-20       35
## 9      0 0010-01-20       40
## 10     1 0010-01-20       45
## 11     0 0010-01-20       50
## 12     0 0010-01-20       55
## 13     0 0010-01-20      100
## 14     1 0010-01-20      105
## 15     0 0010-01-20      110
## 16     0 0010-01-20      115
## 17     0 0010-01-20      120
## 18     1 0010-01-20      125
## 19     2 0010-01-20      130
## 20     0 0010-01-20      135
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newSteps_Day <- aggregate(raw_Data2$steps, by = list(raw_Data2$date), sum)
names(newSteps_Day) <- c("date", "total.Steps")
head(newSteps_Day, 20)
```

```
##          date total.Steps
## 1  0010-01-20       10762
## 2  0010-02-20         126
## 3  0010-03-20       11352
## 4  0010-04-20       12116
## 5  0010-05-20       13294
## 6  0010-06-20       15420
## 7  0010-07-20       11015
## 8  0010-08-20       10762
## 9  0010-09-20       12811
## 10 0010-10-20        9900
## 11 0010-11-20       10304
## 12 0010-12-20       17382
## 13 0011-01-20       10762
## 14 0011-02-20       10600
## 15 0011-03-20       10571
## 16 0011-04-20       10762
## 17 0011-05-20       10439
## 18 0011-06-20        8334
## 19 0011-07-20       12883
## 20 0011-08-20        3219
```

```r
#plot histogram using ggplot2 package
ggplot(newSteps_Day, aes(x = newSteps_Day$total.Steps)) + geom_histogram(fill = "orange", binwidth = 1000) + labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/NH-1.png)<!-- -->


```r
# Mean on New Data
mean(newSteps_Day$total.Steps)
```

```
## [1] 10737.96
```



```r
# Median on New Data
median(newSteps_Day$total.Steps)
```

```
## [1] 10762
```

**Original mean and median**


```r
mean(steps_Day$total.Steps, na.rm = T)
```

```
## [1] 10729.94
```


```r
median(steps_Day$total.Steps, na.rm = T)
```

```
## [1] 10890
```

values of mean and median has changed, new **mean** inceases with **8.02**.


##Are there differences in activity patterns between weekdays and weekends?


```r
#making a new factor variable with values weekday or weekend 
raw_Data2$day.Type <- ifelse (weekdays(raw_Data2$date) == "Saturday" | weekdays(raw_Data2$date) == "Sunday", "Weekend", "Weekday")
head(raw_Data2, 20)
```

```
##    steps       date interval day.Type
## 1      2 0010-01-20        0  Weekday
## 2      0 0010-01-20        5  Weekday
## 3      0 0010-01-20       10  Weekday
## 4      0 0010-01-20       15  Weekday
## 5      0 0010-01-20       20  Weekday
## 6      2 0010-01-20       25  Weekday
## 7      1 0010-01-20       30  Weekday
## 8      1 0010-01-20       35  Weekday
## 9      0 0010-01-20       40  Weekday
## 10     1 0010-01-20       45  Weekday
## 11     0 0010-01-20       50  Weekday
## 12     0 0010-01-20       55  Weekday
## 13     0 0010-01-20      100  Weekday
## 14     1 0010-01-20      105  Weekday
## 15     0 0010-01-20      110  Weekday
## 16     0 0010-01-20      115  Weekday
## 17     0 0010-01-20      120  Weekday
## 18     1 0010-01-20      125  Weekday
## 19     2 0010-01-20      130  Weekday
## 20     0 0010-01-20      135  Weekday
```


```r
#aggregating mean values for each day by day type, then by interval
mean_steps_dayType_interval <- aggregate(raw_Data2$steps, by = list(raw_Data2$day.Type, raw_Data2$interval), mean)

#new names
names(mean_steps_dayType_interval) <- c("day.Type", "interval", "total.Steps")
#using ggplot
ggplot(mean_steps_dayType_interval, aes(x = interval, y = total.Steps, color = day.Type)) + geom_line() + facet_grid(day.Type ~ .) +  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/newPlot-1.png)<!-- -->


There seems to be variation in beginning of **weekdays**, and an overall larger number of steps during **weekends**

##by: Ahmed Talaat

