library(dplyr)
library(ggplot2)
library(lubridate)
unzip("activity.zip")
rawdata <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
mean.steps <- group_by(rawdata, date) 
steps.day = summarise(mean.steps, step.per.day = sum(steps,na.rm = TRUE) )

ggplot(data=steps.day, aes(x=step.per.day)) + 
    geom_histogram() + 
    ggtitle("Histogram of Total Number of Steps Taken per day") + 
    xlab("Steps") + 
    ylab("Counts")
    
mean(steps.day$step.per.day)
median(steps.day$step.per.day)

## What is the average daily activity pattern?
rawdata$interval.index <- (rawdata$interval) %% 1440
daily.pattern <- group_by(rawdata, interval.index) 
steps.interval = summarise(daily.pattern, step.per.interval = mean(steps,na.rm = TRUE) )
ggplot(data = steps.interval, aes(interval.index, step.per.interval)) + 
    geom_line() + 
    labs(title="Average daily activity pattern", x = "Interval", y = "Average Steps")

index = which.max(steps.interval$step.per.interval)

## Imputing missing values
num.NA <- sum(is.na(rawdata$steps))

#filling in all of the missing values, using the mean for that 5-minute interval
#create a new dataset named newdata 
newdata <- rawdata
steps.old <- rawdata$steps
n <- length(steps.old)
for(i in 1:n) {
    if ( is.na(steps.old[i]) ) {
        test = rawdata[i,3] %% 1440 #get the interval value 
        newdata[i,1] <- filter(steps.interval, interval.index == test)$step.per.interval
    }
}

new.mean.steps <- group_by(newdata, date) 
new.steps.day = summarise(new.mean.steps, step.per.day = sum(steps,na.rm = TRUE) )

ggplot(data=new.steps.day, aes(x=step.per.day)) + 
    geom_histogram() + 
    ggtitle("Histogram of Total Number of Steps Taken per day") + 
    xlab("Steps") + 
    ylab("Counts")

mean(new.steps.day$step.per.day)
median(new.steps.day$step.per.day)

## differences in activity patterns between weekdays and weekends
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
tmp <- weekdays(ymd(rawdata$date))
tmp2 <- tmp
tmp2[tmp == "Sunday" | tmp == "Saturday"] <- "weekend"
tmp2[tmp != "Sunday" & tmp != "Saturday"] <- "weekday"

rawdata$week.status <- factor(tmp2)

rawdata.weekday <- filter(rawdata, week.status == "weekday" )
weekday.pattern <- group_by(rawdata.weekday, interval.index) 
weekday.steps.interval = summarise(
    weekday.pattern, step.per.interval = mean(steps,na.rm = TRUE) 
    )

rawdata.weekend <- filter(rawdata, week.status == "weekend" )
weekend.pattern <- group_by(rawdata.weekend, interval.index) 
weekend.steps.interval = summarise(
    weekend.pattern, step.per.interval = mean(steps,na.rm = TRUE) 
)

weekday.steps.interval$week.status <- factor(rep("weekday", times = dim(weekday.steps.interval)[1]))
weekend.steps.interval$week.status <- factor(rep("weekend", times = dim(weekend.steps.interval)[1]))

output <- rbind(weekday.steps.interval,weekend.steps.interval)

ggplot(output, aes(interval.index, step.per.interval) ) +
    facet_wrap(~ week.status, nrow = 2, ncol = 1) +
    geom_line() +
    labs(title="Daily Activity Pattern in weekend and weekday", x = "Interval", y = "Number of steps")