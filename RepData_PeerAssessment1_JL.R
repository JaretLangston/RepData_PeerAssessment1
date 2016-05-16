RunRepData <- function(){
        
        #Read the raw data file
        activity_Raw <- read.csv("activity.csv")
        #presere the raw data for future use and move to a new vairiable for cleaning
        activity_Clean <- activity_Raw
        #Remove any incomplete rows
        activity_Clean <- na.omit(activity_Clean)
        print(paste("Complete Rows: ",as.character(nrow(activity_Clean))))
        print(paste("Number of rows with NA: ",as.character(nrow(activity_Raw) - nrow(activity_Clean))))
        
        
        mysteps <- activity_Clean[,c('steps','interval')]
        #group the dataframe by interval
        mysteps <- dplyr::group_by(mysteps,interval)
        #calculate the total mean steps by interval
        sumSteps <- dplyr::summarise(mysteps, meanSteps = mean(steps))
        activity_Updated <- activity_Raw
        activity_Updated$steps[is.na(activity_Updated$steps)] <- activity_Clean$steps[activity_Clean$interval == activity_Updated$interval]
        print(nrow(activity_Updated))
        
        #create a vector of weekdays
        weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
        activity_Updated$wDay <- c('weekend', 'weekday')[(weekdays(as.Date(activity_Updated$date)) %in% weekdays1)+1L]
        
        mystepsWeekday <- activity_Updated[activity_Updated$wDay == 'weekday',c('steps','interval')]
        mystepsWeekend <- activity_Updated[activity_Updated$wDay == 'weekend',c('steps','interval')]
        #group the dataframe by Interval
        mystepsWeekday <- dplyr::group_by(mystepsWeekday,interval)
        mystepsWeekend <- dplyr::group_by(mystepsWeekend,interval)
        #calculate the total sum of steps by interval
        sumStepsWeekday <- dplyr::summarise(mystepsWeekday, tSteps = sum(steps))
        sumStepsWeekend <- dplyr::summarise(mystepsWeekend, tSteps = sum(steps))
        #setup plot space
        oldpar <- par(mfrow=c(2,1), mar=c(3,3,1,1), oma=c(0,0,3,1))  ## oma creates space 
        plot(sumStepsWeekend$interval,sumStepsWeekend$tSteps, type='l', main="Weekend", xlab = "Interval", ylab = "Total Steps")
        plot(sumStepsWeekday$interval,sumStepsWeekday$tSteps, type='l', main="Weekday", xlab = "Interval", ylab = "Total Steps")
        mtext("Average steps for weekend and weekday", side=3, line=1, outer=TRUE, cex=2, font=2)
        par(oldpar)
        summary(sumSteps)
        #         #get only the steps and date columns
#         mysteps <- activity_Clean[,c('steps','date')]
#         #group the dataframe by date
#         mysteps <- dplyr::group_by(mysteps,date)
#         #calculate the total sum of steps, mean steps, and median steps by date
#         sumSteps <- dplyr::summarise(mysteps, tSteps = sum(steps),meanSteps = mean(steps), medianSteps = median(steps))
#          #Plot histogram of total steps per day
#        plot(sumSteps$date,sumSteps$tSteps, type = 'h',
#              xlab = "Date",ylab = "Total Steps", 
#              main = "Histogram of Total Steps per Day")
#        #Plot histogram of mean steps per day
#        plot(sumSteps$date,sumSteps$meanSteps, type = 'h',
#             xlab = "Date",ylab = "Mean Steps", 
#             main = "Histogram of Mean Steps per Day")
#        #Plot histogram of median steps per day
#        plot(sumSteps$date,sumSteps$medianSteps, type = 'h',
#             xlab = "Date",ylab = "Median Steps", 
#             main = "Histogram of Median Steps per Day")
       
        
        
}