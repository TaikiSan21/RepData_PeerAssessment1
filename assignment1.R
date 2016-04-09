## Testing code for Rep Research assignment 1

## Loading and pre-processing. No processing done.
setwd('~/Coursera/DS Repo/RepResearch/RepData_PeerAssessment1')

library(data.table)
actData <- data.table(read.csv('activity.csv'))
## Convert date string to date object
actData$posDate <- as.POSIXct(actData$date, format='%Y-%m-%d')
## What is mean total number of steps taken per day?
daySum <- actData[, list(sum=sum(steps)),by=posDate]

hist(daySum$sum)

list(mean=mean(daySum$sum, na.rm=TRUE), median=median(daySum$sum, na.rm=TRUE))
## What is the average daily activity pattern?
## get means grouped by interval
intAvg <- actData[,list(mean=mean(steps, na.rm=TRUE)),by=interval]
## Find the interval with the max
intAvg[mean==max(intAvg$mean),]

## Imputing missing values
sum(is.na(actData$steps))

## I want to use average for that interval because interval seems to have strong
## relationship with steps, where day is more random. Also we are missing entire
## days, so this will fill those gaps in. 
impData <- actData
for(i in seq_along(impData$steps)) {
      if(is.na(impData$steps[i])) {
            impData$steps[i] <- intAvg$mean[intAvg$interval == impData$interval[i]] }
}

impSum <- impData[, list(sum=sum(steps)),by=posDate]
list(mean=mean(impSum$sum, na.rm=TRUE), median=median(impSum$sum, na.rm=TRUE))

## Are there differences in activity patterns between weekdays and weekends?
impData$dayType <- factor(weekdays(impData$posDate)%in%c('Saturday','Sunday'), labels=c('weekday','weekend'))
## get means broken up by interval and weekday
impInt <- impData[,list(mean=mean(steps)),by=c('interval','dayType')]
xyplot(mean~interval|dayType, data=impInt, type='l')
