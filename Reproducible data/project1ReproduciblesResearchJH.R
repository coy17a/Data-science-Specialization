library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

data <- read_csv("activity.csv")
str(data)
data$date <- ymd(data$date)
day_steps <- data %>%
  filter(!is.na(steps))%>%
  group_by(date) %>%
  summarize(steps_per_day=sum(steps),
            avg_per_day = mean(steps))

ggplot(day_steps)+
  aes(steps_per_day)+
  geom_histogram(fill="steelblue",binwidth = 2000)

avg_spd <- mean(day_steps$steps_per_day)
median_spd <- median(day_steps$steps_per_day)

five_minutes_intervals <- data %>%
  filter(!is.na(steps))%>%
  group_by(interval)%>%
  summarise(fiveallday = mean(steps))

qplot(interval,fiveallday, data=five_minutes_intervals,geom = "line")
max_interval <- filter(five_minutes_intervals,fiveallday== max(five_minutes_intervals$fiveallday))

totalna <- data %>%
  filter(is.na(steps))%>%
  summarize(n())
totalna[1]

data2 <- data
data2$avg_interval = unlist(sapply(data2$interval, function(x) five_minutes_intervals[x== five_minutes_intervals$interval,2]))

my.na <- is.na(data2$steps)
data2$steps[my.na] <- data2$avg_interval[my.na]

day_steps2 <- data2 %>%
  group_by(date) %>%
  summarize(steps_per_day=sum(steps),
            avg_per_day = mean(steps))

ggplot(day_steps2)+
  aes(steps_per_day)+
  geom_histogram(fill="steelblue",binwidth = 2000)

avg_spd2 <- mean(day_steps2$steps_per_day)
median_spd2 <- median(day_steps2$steps_per_day)

data2$day <- wday(data2$date)
data2$day <- sapply(data2$day , function(x) ifelse((x==6 | x== 7),"weekend","weekday"))
data2$day <- as.factor(data2$day)

avg_steps_all <- data2 %>%
  group_by(interval,day) %>%
  summarize(avg_steps = mean(steps))

ggplot(avg_steps_all)+
  aes(interval, avg_steps)+
  geom_line()+
  facet_grid(day~.)
