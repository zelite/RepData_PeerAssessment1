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
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
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



## Are there differences in activity patterns between weekdays and weekends?
