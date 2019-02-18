library(dplyr)
library(lubridate)
#Read data and select data betweem 2007-02-01 to 2007-02-02

data <- read.table("household_power_consumption.txt", sep = ";", header = T, stringsAsFactors = FALSE)
data$Date <- dmy(data$Date)
data_sel <- subset(data, Date == "2007-02-01" | Date == "2007-02-02")
data_sel <- na.omit(data_sel)
data_sel[3:9] <- sapply(data_sel[3:9], as.numeric)
data_sel$Datetime <- ymd_hms(paste(data_sel$Date, data_sel$Time))

#plotting
png("plot3.png")
plot(data_sel$Datetime, data_sel$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(data_sel$Datetime, data_sel$Sub_metering_2, col ="red")
lines(data_sel$Datetime, data_sel$Sub_metering_3, col ="blue")
legend("topright", legend = c(colnames(data_sel[7:9])), col = c("black", "red", "blue"), lty = 1 )
dev.off()

##example 2
data<-read.table('household_power_consumption.txt', header=T, sep=';', na.strings="?", 
                 nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')

data$Date<-as.Date(data$Date,format = '%d/%m/%Y')

work<-subset(data,subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))

data2 <- paste(as.Date(work$Date), work$Time)
work$Datetime <- as.POSIXct(data2)

plot(work$Global_active_power~work$Datetime, type="l",
     ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()