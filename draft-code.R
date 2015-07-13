library(dplyr)

data <- read.csv("data/activity.csv")

# Average (mean) number of steps by date
by_date <- group_by(data,date )
sum_steps <- summarise(by_date,stps=sum(steps))
hist(sum_steps$stps, breaks=15)


