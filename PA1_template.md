---
title: "Peer Assigment 1"
author: "Sean Treadway"
date: "June 14, 2015"
output: html_document
---

Report for Peer Assignment #1 in Reproduceable Research

The data to be analysed and reported on is 2 month of pedometer data from an anonymouns individual.  The data was gathered over 2 months, and the actual "step" are binned in to 5-minute interval.  There are a total of 17568 5-minute interval records.  Each record consisted of a "step", "data", and "interval" entry.

The data provided as part of the assignment.

Here the data is read in to R.

```r
#Set number of record/lines.  Given in instructions, checked with 'wc' at command line
records=17568
#Set the colume classes
classes<-c("numeric","Date","numeric")
#Set column names.  Comes from header
cnames=c("steps","date","interval")
#Read in data
alldata<-read.delim("../activity.csv",header=TRUE,sep=",", nrows=records,colClasses=classes,strip.white=TRUE)
```

The first part of the assigment was to determine the mean, and median daily steps, and to plot a histogram of daily steps.
NAs in the step entries were ignored for this part of the analysis.

```r
#Create a logical vector for selecting records with out step NAs
goodrows<-!is.na(alldata$steps)
#Create total steps per day vector
daily<- as.vector(tapply(alldata[goodrows,]$steps,alldata[goodrows,]$date,sum))
#Determine the mean and median daily steps, ignoring NAs
dailymean<-mean(daily)
dailymedian<-median(daily)
dailymean
```

```
## [1] 10766.19
```

```r
dailymedian
```

```
## [1] 10765
```
The mean total steps was 10766.189, and the median 10765.

Just below is the histogram for total daily steps.  NA were omitted.

```r
#Create histogram showing daily step count frequency
hist(daily,main="Histogram of Daily Step Count",xlab="Steps/Day",ylab="Number of Days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The greatest number of step was typically taken between 835 and 840 AM, with 104 steps on average for the entire 2 month period.  NAs were omitted.

```r
#Determine the mean for each interval.  This will be used to fill in the step NAs.  
#This totally ignores day/week/month differences that might exist, and any trends in activity. 
#Ignoring these simplifies the analysis for the sake of the exercise.
intmeans24<-tapply(alldata$steps,alldata$interval,mean,na.rm=TRUE)
#Determine what interval sees the highest mean number of steps
which.max(intmeans24)
```

```
## 835 
## 104
```

Here is a plot of average steps for each interval.  

```r
#Plot the mean number of steps on an interval basis
plot(intmeans24,type='l',xlab="Interval (5-minute increment)",ylab="Mean Steps",main="Interval Step Average Across 2 Months")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The data is modified to substitute something meaningful in for the 2304 NA step entries.
I chose average for each interval across the 2 month period. Zero (0) was assumed in the case of NA for intervals across the entire time.

```r
sub<-function(record){
        
        if(is.na(record[1])) {
                record[1]<-intmeans24[as.character(record[2])]
        }
        return(record[1])
}
moddata<-alldata
moddata$steps<-apply(subset(alldata, select=c(steps,interval)),1,sub)

moddaily<- as.vector(tapply(moddata$steps,moddata$date,sum))
#Count of NA values
NAcount<-records-sum(goodrows)
moddailymean<-mean(moddaily)
moddailymean
```

```
## [1] 10766.19
```

```r
moddailymedian<-median(moddaily)
moddailymedian
```

```
## [1] 10766.19
```
The modified daily mean, 10766.189 is basically identical to the unmodified data mean.
The modified daily median is up slightly to 10766.189.

Histogram of modified daily steps is just below.

```r
hist(moddaily,main="Histogram of Daily Step Count For Modified Data",xlab="Steps/Day",ylab="Number of Days")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Modification of the data resulted in basically no change in the data trends.  Day frequency count is up, reflecting a more complete data record.