activity <- read.csv("activity.csv",stringsAsFactors = F)
head(activity)

avgspi <- group_by(activity,interval) %>% summarise(steps = round(mean(steps,na.rm =T )))


fillNAs <- function(activity,avgspi){
  interval <- avgspi$interval
  for( i in interval){
    activity[(activity$interval == i) & is.na(activity$steps),]$steps <- avgspi[avgspi$interval == i,]$steps
  }
  return(activity)
}