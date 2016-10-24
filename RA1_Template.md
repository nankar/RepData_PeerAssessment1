``` r
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
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(ggplot2)
library(data.table)
```

    ## -------------------------------------------------------------------------

    ## data.table + dplyr code now lives in dtplyr.
    ## Please library(dtplyr)!

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, last

``` r
#Loading and preprocessing the data
activity_dataset <- read.csv("activity.csv")
#head(activity_dataset)

activity_dataset$date <- as.Date(activity_dataset$date, format = "%Y-%m-%d")
activity_dataset$interval <- as.factor(activity_dataset$interval)
#head(activity_dataset$date)

#Question 1. What is mean total number of steps taken per day?

totalperday.steps <- tapply(activity_dataset$steps, activity_dataset$date, FUN=sum, na.rm=TRUE)

#draw graphics
qplot(totalperday.steps, binwidth=1000, fill = "brown", main="Graph of steps taken per day " ,xlab="total number of steps taken each day", ylab="Number of times in a day")
```

![](RA1_Template_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#calculate mean and median of steps per day
mean(totalperday.steps, na.rm=TRUE)
```

    ## [1] 9354.23

``` r
median(totalperday.steps, na.rm=TRUE)
```

    ## [1] 10395

``` r
#Question 2.What is the average daily activity pattern?

daily_average <- aggregate(x=list(steps=activity_dataset$steps), by=list(interval=activity_dataset$interval),
                      FUN=mean, na.rm=TRUE)
#draw graphics
ggplot(daily_average, aes(x = interval, y = steps, group = 1)) + 
  geom_line() + scale_x_discrete(breaks = seq(0, 3000, 500))
```

![](RA1_Template_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
#calculate max of daily average activity pattern
daily_average[which.max(daily_average$steps),]
```

    ##     interval    steps
    ## 104      835 206.1698

``` r
# Question 3.Imputing missing values

#number of missing values
missing_values <- sum(is.na(activity_dataset))
table(missing_values)
```

    ## missing_values
    ## 2304 
    ##    1

``` r
na_pos <- which(is.na(activity_dataset$steps))

# Create a vector of means of steps
mean_vector <- rep(mean(activity_dataset$steps, na.rm=TRUE), times=length(na_pos))

# Replace the NAs by the means vector
activity_dataset[na_pos, "steps"] <- mean_vector

#aggregate data of steps by each day
final_sum_data <- aggregate(activity_dataset$steps, by=list(activity_dataset$date), FUN=sum)

# Rename the attributes
names(final_sum_data) <- c("date", "total")

# draw graphics
ggplot(final_sum_data, aes(x=total)) + 
  geom_histogram(fill = "green", binwidth = 1000) + 
  labs(title="Histogram of the total number of steps taken each day-NA replaced by mean", 
       x = "Number of Steps per Day", y = "Number of times in a day")
```

![](RA1_Template_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
#calculate mean and median for final processed data
mean(final_sum_data$total)
```

    ## [1] 10766.19

``` r
median(final_sum_data$total)
```

    ## [1] 10766.19

``` r
# Question 4.Are there differences in activity patterns between weekdays and weekends?

dataset_days <- activity_dataset %>%
  mutate(type_of_day = as.factor(format(date,"%a")))

dataset_days$type_of_day <- revalue(dataset_days$type_of_day, c("Mon"="Weekday", "Tue"="Weekday", "Wed"="Weekday", 
                                                                "Thu"="Weekday", "Fri"="Weekday", "Sat"="Weekend", 
                                                                "Sun"="Weekend"))

dataset_weekday <- subset(dataset_days, type_of_day == "Weekday")
dataset_weekend <- subset(dataset_days, type_of_day == "Weekend")

weekday_avg <- aggregate(dataset_weekday$steps, list(dataset_weekday$interval,dataset_weekday$type_of_day), mean)
weekend_avg <- aggregate(dataset_weekend$steps, list(dataset_weekend$interval,dataset_weekend$type_of_day), mean)

colnames(weekday_avg) <- c("interval", "type_of_day", "avg_steps")
colnames(weekend_avg) <- c("interval", "type_of_day", "avg_steps")

weekday_data <- rbind(weekday_avg, weekend_avg)

#draw graphics
ggplot(weekday_data, aes(x=interval, y=avg_steps, group=1)) + geom_line() + 
  scale_x_discrete(breaks=seq(0,2500,500)) + 
  facet_wrap(~ type_of_day, nrow=2) + 
  ylab("Number of steps")
```

![](RA1_Template_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
knitr::opts_chunk$set(echo = TRUE)
```

------------------------------------------------------------------------
