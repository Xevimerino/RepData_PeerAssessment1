#Unzipping and reading the data
unzip("activity.zip", exdir="./Data")
activity<-read.csv("./Data/activity.csv")

#Converting the interval column into time format
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(activity[,"interval"]))
activity[,"interval"]<-paste0(temp2, activity[,"interval"])
activity[,"interval"]<-format(strptime(activity[,"interval"], format="%H%M"), format = "%H:%M")

#Converting the date column into date format
activity[,"date"]<-strftime(activity[,"date"],format="%Y-%m-%d")

#calculate nºsteps per day
steps<-mapply(function(x) sum(activity[activity[,"date"]==x,"steps"],na.rm=T), dates)
hist(steps, xlab="number of steps per day",main="Steps per day")

#Calculate mean for each day
means<-mapply(function(x) mean(activity[activity[,"date"]==x,"steps"],na.rm=T), dates)
hist(means, xlab="mean steps per day",main="Mean steps per day")

?hist
