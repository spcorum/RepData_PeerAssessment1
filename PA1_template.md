---
title: "Reproducible Research: Peer Assessment 1"
author: "Sean Corum"
date: "May 11, 2014"
output: html_document
---

## Loading and preprocessing the data
The R code below obtains the data.  The "...data [are] from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

```{r}
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(url, "activity.zip", method = 'curl')
unzip('activity.zip')
time <- format(Sys.time(), '%a %b %d %X %Y')
data <- read.csv('activity.csv')
str(data)
```

The data were obtained from `r url` on `r time`.

## What is mean total number of steps taken per day?

This can be computed by the following R code:
```{r}
sumPerDay <- aggregate(steps ~ date, data = data, sum, na.action = na.omit)
meanSum <- mean(sumPerDay$steps, na.rm = TRUE)
meanSum
medianSum <- median(sumPerDay$steps, na.rm = TRUE)
medianSum
```

Ignoring missing values, the mean and median total steps per day are `r as.integer(meanSum)` and `r medianSum`, respectively.

The following is a histogram of the total steps per day:

```{r}
with(sumPerDay, hist(steps))
```

## What is the average daily activity pattern?

The following code makes a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
averagePerInterval <- aggregate(steps ~ interval, data = data, mean,
                                na.action = na.omit)
with(averagePerInterval, plot(interval, steps, type = 'l'))
```

The 5-minute interval that contains the maximum number of steps on average is given by:

```{r}
argmaxInterval = averagePerInterval[max(averagePerInterval$steps) == averagePerInterval$steps,1]
argmaxInterval
```
The fact that the most stepping activity occurs at 0`r argmaxInterval %/% 100`:`r argmaxInterval %% 100` AM likely corresponds to the commute to work.

## Imputing missing values

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is given by:

```{r}
sum(is.na(data$steps))
```

The missing step values in the data set may be imputed by replacing them as with the median value of the same 5-minute interval across all days. A new dataset with the imputed values using this strategy may be constructed by using tapply to compute the median within each interval (treating intervals as factors and ignoring NAs). The output of tapply can be coerced to a named list, with the names of the list elements being the interval values, which can then be matched to the original missing step data for imputation by replacement with the intra-interval, inter-day median.

```{r}
# Create copy of original data.
newdata <- data

# This application of tapply calculates the median step value over each 5-minute interval across all days, disregarding NAs. The output is a named list of median values, with the names being the associated interval.
medianPerInterval <- as.list(with(newdata, tapply(steps, as.factor(interval), median, na.rm=TRUE)))

# This for loop replaces missing step values with the associated interval median, thereby imputing the missing values.
for (i in 1:length(medianPerInterval)) {
    myLogical <- with(newdata, is.na(steps) & interval == names(medianPerInterval[i]))
    newdata$steps[myLogical] <- as.integer(medianPerInterval[i])
}

# Check that no NAs are left in the newdata step values
sum(is.na(newdata$steps)) 
```

Thus, all missing step values have been removed, and the "newdata" dataframe is an exact copy of the original data, except all missing step values have been replaced by the median values as described above. With this new data, the total number of steps taken per day, as well as the mean and median total steps per day, may be computed as: 

```{r}
newSumPerDay <- aggregate(steps ~ date, data = newdata, sum)
newMeanSum <- mean(newSumPerDay$steps)
newMeanSum
newMedianSum <- median(newSumPerDay$steps)
newMedianSum
```

With imputed missing values, the mean and median total steps per day are `r as.integer(newMeanSum)` and `r newMedianSum`, respectively, which deviate below the original values by `r abs(newMeanSum-meanSum)/meanSum*100`% and `r abs(newMedianSum-medianSum)/medianSum*100`%, respectively.

The following is a histogram of the total steps per day the data with imputed missing values:

```{r}
with(newSumPerDay, hist(steps))
```

As could be expected, the mean and median shift downward, which is explained by the extra lower-valued step values imputed into the data set.

## Are there differences in activity patterns between weekdays and weekends?
A new factor variable may be created with two levels – “weekday” and “weekend”  - indicating whether a given date is a weekday or weekend day using the following R code:

```{r}
newdata$daytype <- weekdays(as.Date(as.character(newdata$date), '%Y-%m-%d'))
newdata$daytype[grep('Monday|Tuesday|Wednesday|Thursday|Friday',newdata$daytype)] <-   'weekday'
newdata$daytype[grep('Saturday|Sunday',newdata$daytype)] <- 'weekend'
newdata$daytype <- as.factor(newdata$daytype)
```

Then, the average across each interval for each type of day (weekday or weekend) may be found and plotted.
```{r}
averagePerIntervalAndDayType <- aggregate(steps ~ interval + daytype, data = newdata, mean)
library(ggplot2)
qplot(interval, steps, data = averagePerIntervalAndDayType, facets = daytype ~., geom = 'line')
```

By inspection, the answer seems to be yes, there are differences in the average  pattern of steps per time on weekdays vs. weekends, with the most notible difference being a more variable number of steps being taken at different times throughout the day on the weekends. A likely explanation is that walking activity is less habitual over time on weekends than on weekdays.
