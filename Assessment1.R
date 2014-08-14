#Setting the local language to english, to work with weekdays
Sys.setlocale("LC_TIME", "English")

#Unzipping and reading the data
unzip("activity.zip", exdir="./Data")
activity<-read.csv("./Data/activity.csv")

#Storing the activity intervals as a variable for later use
intervalsnum<-as.numeric(unique(activity[,"interval"]))
#Converting the interval column into time format
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(activity[,"interval"]))
activity[,"interval"]<-paste0(temp2, activity[,"interval"])
activity[,"interval"]<-format(strptime(activity[,"interval"], format="%H%M"), format = "%H:%M")

#Converting the date column into date format
activity[,"date"]<-strftime(activity[,"date"],format="%Y-%m-%d")

#Getting vectors with all the diferent dates and times 
dates<-as.Date(unique(activity[,"date"]))
intervals<-unique(activity[,"interval"])

#calculate nºsteps per day, histogram, mean and median
steps<-mapply(function(x) sum(activity[activity[,"date"]==x,"steps"],na.rm=T), dates)
hist(steps, xlab="number of steps per day",main="Steps per day")
mean(steps)
median(steps)

#Calculate mean nº of steps at each time point
stepstime<-mapply(function(x) mean(activity[activity[,"interval"]==x,"steps"],na.rm=T), intervals)
plot(intervalsnum,stepstime,type="l",axes=FALSE,xlab="Time of the day",ylab="Mean number of steps across all days")
  axis(side=1,at=c(0,500,1000,1500,2000,2359), labels=c("00:00","05:00","10:00","15:00","20:00","23:59"))
  axis(side=2,at=c(0,50,100,150,200), c(0,50,100,150,200))
#At which time of the day does the maximum steps happen
which.max(stepstime)

#How many rows have NA's'
Nodata<-is.na(activity[,"steps"])
length(Nodata[Nodata==TRUE])

#A vector is created where all the "NA" values are substituted by the mean of the
#same time point across all days
correctedsteps <- ifelse(Nodata, stepstime, activity[,"steps"])

#New dataset changing the steps column by correctedsteps
correctedactivity<-cbind(correctedsteps,activity[,c(2,3)])
correctedactivity[,"date"]<-as.Date(correctedactivity[,"date"])

#calculate nºsteps per day, histogram, mean and median
steps2<-mapply(function(x) sum(correctedactivity[correctedactivity[,"date"]==x,"correctedsteps"],na.rm=T), dates)
hist(steps2, xlab="number of steps per day",main="Steps per day")
mean(steps2)
median(steps2)

#Coonverting the dates to weekdays and weekends
correctedactivity[,"date"]<-weekdays(correctedactivity[,"date"])
correctedactivity[,"date"]<- ifelse(correctedactivity[,"date"]=="Saturday", "weekend", correctedactivity[,"date"])
correctedactivity[,"date"]<- ifelse(correctedactivity[,"date"]=="Sunday", "weekend", correctedactivity[,"date"])
correctedactivity[,"date"]<- as.factor(ifelse(correctedactivity[,"date"]=="weekend", correctedactivity[,"date"],"weekday"))

CAweekday<-correctedactivity[correctedactivity["date"]=="weekday",]
CAweekend<-correctedactivity[correctedactivity["date"]=="weekend",]

#CODE TO REVISE
par(mfcol=c(2,1),cra=c(300,700))
stepstimeWD<-mapply(function(x) mean(CAweekday[CAweekday[,"interval"]==x,"correctedsteps"],na.rm=T),SIMPLIFY=F, intervals)
    plot(intervalsnum,stepstimeWD,type="l",axes=FALSE,
         xlab="Time of the day",ylab="")
    axis(side=1,at=c(0,500,1000,1500,2000,2359), 
         labels=c("00:00","05:00","10:00","15:00","20:00","23:59"))
    axis(side=2,at=c(0,50,100,150,200,250), c(0,50,100,150,200,250))
stepstimeWE<-mapply(function(x) mean(CAweekend[CAweekend[,"interval"]==x,"correctedsteps"],na.rm=T),SIMPLIFY=F, intervals)
    plot(intervalsnum,stepstimeWE,type="l",axes=FALSE,
         xlab="Time of the day",ylab="")
    axis(side=1,at=c(0,500,1000,1500,2000,2359), 
         labels=c("00:00","05:00","10:00","15:00","20:00","23:59"))
    axis(side=2,at=c(0,50,100,150,200,250), c(0,50,100,150,200,250))

x<-as.data.frame(cbind(intervals,stepstimeWD,date="weekday"),colnames=c("intervals","stepstimeWE","date"))
y<-as.data.frame(cbind(intervals,stepstimeWE,date="weekend"))
colnames(y[2])<-"steptime"
colnames(x[2])<-"steptime"
z<-rbind(x,y)
names(x)

install.packages("lattice")
library(lattice)

xyplot( ~ interval, data=correctedactivity, type='b')
  ?xyplot
