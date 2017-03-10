library(ggplot2)
library(lattice)
library(dplyr)
library(magrittr)
library(purrr)



activity_data <- read.csv("activity.csv",stringsAsFactors = FALSE,header = TRUE)
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")

steps_eachday <- activity_data %>%
                            group_by(date) %>%
                            summarise(step_taken = sum(steps,na.rm = TRUE))

png("plot1.png")

ggplot(steps_eachday,aes(step_taken)) + geom_histogram(col = "black",fill = "blue") +
    xlab("Steps taken Each Day") + ylab("Counts") + ggtitle("Total No. of Steps taken each day")

dev.off()

summary(steps_eachday$step_taken)


mean_day <- mean(steps_eachday$step_taken,na.rm = TRUE)
median_day <- median(steps_eachday$step_taken,na.rm = TRUE)

avg_step_takenby_interval <- activity_data %>%
                                group_by(interval) %>%
                                summarise(avg_step = mean(steps,na.rm = TRUE))

ggplot(avg_step_takenby_interval,aes(interval,avg_step)) + geom_line(col = "red") + 
    xlab("Interval") + ylab("Average Steps by Interval") + ggtitle("Time-Series Plot")


max_step_byinterval <- arrange(avg_step_takenby_interval,desc(avg_step))


total_missing <- sum(is.na(activity_data))

activity_data[is.na(activity_data)] <- avg_step_takenby_interval$avg_step

activity2 <- activity_data

by_dy_step <- activity2 %>%
                    group_by(date) %>%
                    summarise(step = sum(steps)) 


ggplot(by_dy_step,aes(step)) + geom_histogram(fill = "red",col = "black")
mean_noNA <- mean(by_dy_step$step)
median_noNA <- median(by_dy_step$step)

summary(by_dy_step$step)
summary(steps_eachday$step_taken)

weekday <- function(date) {
    day <- weekdays(date)
    if(day %in% c("Sunday","Saturday")){
        print("Weekend")
    }
    else{
        print("Weekday")
    }
}
activity2 <- activity2 %>%
                    mutate(day = map(date,weekday))

activity2$day <- unlist(activity2$day)
activity2$day <- as.factor(activity2$day)

by_interval <- activity2 %>%
                group_by(interval,day) %>%
                summarise(step = sum(steps))


ggplot(by_interval,aes(interval,step)) + geom_line(col = "red") + facet_grid(day ~.)











