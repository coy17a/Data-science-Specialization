library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)

emissions_baltimore <- NEI %>%
  filter(fips == "24510")%>%
  group_by(year)%>%
  summarise(Emissions = sum(Emissions))

png("plot2.png")
barplot(emissions_baltimore$Emissions,names= emissions_baltimore$year, col= c("red"),
        main = "Total PM2.5 emissions from all Sources In Baltimore City",
        xlab= "Year", ylab = "PM2.5 emittet in Tons")
dev.off()