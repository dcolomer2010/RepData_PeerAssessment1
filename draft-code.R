library(plyr)
library(pander)

rm(list=ls())

data <- read.csv("data/activity.csv")
data$date <- as.Date(data$date)

# Average (mean) number of steps by date
by_date <- data
#by_date <- group_by(data , date )
#sum_steps <- summarise(by_date,stps=sum(steps))
sum_steps <- ddply(by_date,c("date"),summarise, stps = sum(steps))
count_steps <- ddply(by_date,c("date"),summarise, N = length(steps))

mean_steps <- ceiling(mean(sum_steps$stps,na.rm=TRUE))
median_steps <- ceiling(quantile(sum_steps$stps,0.5,na.rm = TRUE))

hist(sum_steps$stps, breaks=15)

interval_means <- ddply(by_date, c("interval"),summarise, stps=mean(steps,na.rm=TRUE))
plot(interval_means$stps,type="l")

#####
interval_data <- ddply(data,c("interval"),summarise, mean_steps <- mean(steps))
data2<-data[order(data$date,data$interval),]
plot(data2$steps,type="l")

na.omit(data2)
maximum <- data2[data2$steps==max(data2$steps,na.rm=TRUE),]
na.omit(maximum)

print(maximum)

#### Missing values
dataMunged <- data
for (x in 1:nrow(dataMunged)) {
  if (is.na(dataMunged$steps[x])) {
    interval_index <- dataMunged$interval[x]/5 + 1
    dataMunged$steps[x] <- interval_means$stps[interval_index]
  } 
}


#### Week days
weekday <- weekdays(dataMunged$date,abbreviate=FALSE)
for (x in 1:length(weekday)) {
  if (weekday[x] %in% c("sábado","domingo")) 
    weekday[x] <- "weekend"
  else
    weekday[x] <- "weekday"
}

dataMunged <- cbind(dataMunged, weekday)

wd_steps <- ddply(dataMunged,c("weekday","interval"),summarise, mean_stps = mean(steps,na.rm = TRUE))

steps_weekday <- wd_steps[wd_steps$weekday=="weekday",]
steps_weekend <- wd_steps[wd_steps$weekday=="weekend",]

par(mfrow=c(2,1))
plot(steps_weekday$mean_stps,type="l")
plot(steps_weekend$mean_stps,type="l")
par(mfrow=c(1,1))




