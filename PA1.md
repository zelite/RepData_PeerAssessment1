---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(magrittr)
library(lubridate)

unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity <- mutate(activity, date = ymd(date))
```



## What is mean total number of steps taken per day?



```r
steps_per_day <- activity %>% 
                 group_by(date) %>%
                 summarise(total_steps = sum(steps, na.rm = TRUE)) %>%
                 use_series(total_steps)


hist(x = steps_per_day)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(steps_per_day)
```

```
## [1] 9354.23
```

```r
median(steps_per_day)
```

```
## [1] 10395
```


## What is the average daily activity pattern?



```r
steps_per_time <- activity %>%
                  group_by(interval) %>%
                  summarise(average_steps = mean(steps, na.rm = TRUE))

plot(average_steps ~ interval, data = steps_per_time, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
steps_per_time[[which.max(steps_per_time$average_steps), "interval"]]
```

```
## [1] 835
```

## Imputing missing values

There are several R packages that help with imputing missing values. However, lets keep it simple, as the instruction suggest.
First I thought taking the average of each day. However there are days without any value, which will have `NaN` as mean:




```r
average_steps_day <- activity %>%
                     group_by(date) %>%
                     summarise(average = mean(steps, na.rm = TRUE))

sum(is.nan(average_steps_day$average))
```

```
## [1] 8
```

So, instead lets take the average for the 5 minute interval:


```r
# We have already calculated the average steps per interval in a previous
# question

activity <- activity %>%
            left_join(steps_per_time, by = "interval") 

#I wish I would find a way to do this with dplyr instead :(

for(i in seq_along(activity$steps)){
  activity$steps[i] <- ifelse(is.na(activity$steps[i]), 
                              activity$average_steps[i], 
                              activity$steps[i])
}
```



## Are there differences in activity patterns between weekdays and weekends?
