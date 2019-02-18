library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)

emission_by_years <- NEI %>%
  group_by(year)%>%
  summarise(Emissions = sum(Emissions))

png("plot1.png")
barplot(emission_by_years$Emissions,names= emission_by_years$year, col= c("blue"),
        main = "Total PM2.5 emissions from all Sources in USA",
        xlab= "Year", ylab = "PM2.5 emittet in Tons")
dev.off()
emissions_baltimore <- NEI %>%
  filter(fips == "24510")%>%
  group_by(year)%>%
  summarise(Emissions = sum(Emissions))