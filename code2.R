

#Code for Peer1 Assignment in RepoResearch
#This code will end up in a Markdown document

setwd("~/Documents/DS_track/Repo/peer1")

#Set number of record/lines.  Given in instructions, checked with 'wc' at command line
records=17568
#Set the colume classes
classes<-c("numeric","Date","numeric")
#Set column names.  Comes from header
cnames=c("steps","date","interval")

#Read in data
alldata<-read.delim("./activity.csv",header=TRUE,sep=",", nrows=records,colClasses=classes,strip.white=TRUE)
#Create a logical vector for selecting records with out step NAs
goodrows<-!is.na(alldata$steps)
#Create total steps per day vector
daily<- as.vector(tapply(alldata[goodrows,]$steps,alldata[goodrows,]$date,sum))
#Create histogram showing daily step count frequency
hist(daily,main="Histogram of Daily Step Count",xlab="Steps/Day",ylab="Number of Days")
#Determine the mean and median daily steps, ignoring NAs
dailymean<-mean(daily)
dailymedian<-median(daily)


#Create a factor for day-of-week section.
alldata$day<-weekdays(alldata$date)

#Determine the mean for each interval.  This will be used to fill in the step NAs.  
#This totally ignores day/week/month differences that might exist, and any trends in activity. 
#Ignoring these simplifies the analysis for the sake of the exercise.
intmeans24<-tapply(alldata$steps,alldata$interval,mean,na.rm=TRUE)
#Determine what interval sees the highest mean number of steps
which.max(intmeans24)
#Plot the mean number of steps on an interval basis
plot(intmeans24,type='l')

subset(alldata,day=="Monday" & interval==0)

sub<-function(record){
        
        if(is.na(record[1])) {
                record[1]<-intmeans24[as.character(record[2])]
        }
        return(record[1])
}

subset(alldata, select=c(steps,interval))
moddata<-alldata
moddata$steps<-apply(subset(alldata, select=c(steps,interval)),1,sub)

moddaily<- as.vector(tapply(moddata$steps,moddata$date,sum))
hist(moddaily,main="Histogram of Daily Step Count For Modified Data",xlab="Steps/Day",ylab="Number of Days")
moddailymean<-mean(moddaily)
moddailymedian<-median(moddaily)

NAcount<-records-sum(goodrows)
