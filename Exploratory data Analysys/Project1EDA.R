library(downloader)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)
#import data to data_Frame
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download(fileUrl, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
data <- read.table("./household_power_consumption.txt",sep= ";",header =TRUE)
head(data)

#cleaning and wrangling data 
data$Date <- paste(data$Date , data$Time, sep =" ")
data$Date <- dmy_hms(data$Date)
datefilter <-  (date(data$Date) >= as.Date("2007-02-01") & date(data$Date) <as.Date("2007-02-03"))
data2 <- data[datefilter,]


data2 <- data2 %>%
  mutate_at(c(3:8), as.character)%>%
  mutate_at(c(3:8), as.numeric)

  ggplot(data2)+
  aes(x = Global_active_power)+
  geom_histogram( bins=18,color="black", fill = "red")+
  labs(title = "Global Active Power", x = "Global Active Power (kilowatts)", y = "Frequency")

 p1 <-  ggplot(data = data2)+
  aes( x = Date , y = Global_active_power)+
  geom_line()+
  scale_x_datetime(breaks=c( ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 00:00:00"),ymd_hms("2007-02-02 23:59:00")) ,labels =c("Thurs","Friday","Sat"))
 
 data2_long <- gather(data2,"sub_metering","energy", 7:9)

p2 <- ggplot(data = data2_long)+
  aes( x = Date , y = energy, color = sub_metering)+
  geom_line()+
  theme(legend.position=c(0.8,0.8))+
  scale_x_datetime(breaks=c( ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 00:00:00"),ymd_hms("2007-02-02 23:59:00")) ,labels =c("Thurs","Friday","Sat"))


p3 <- ggplot(data = data2)+
  aes( x = Date , y = Voltage)+
  geom_line()+
  scale_x_datetime(breaks=c( ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 00:00:00"),ymd_hms("2007-02-02 23:59:00")) ,labels =c("Thurs","Friday","Sat"))


p4 <- ggplot(data = data2)+
  aes( x = Date , y = Global_reactive_power)+
  geom_line()+
  scale_x_datetime(breaks=c( ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-02 00:00:00"),ymd_hms("2007-02-02 23:59:00")) ,labels =c("Thurs","Friday","Sat"))


 grid.arrange(p1,p3,p2,p4 ,nrow=2)
  





