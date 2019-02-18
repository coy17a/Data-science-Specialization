library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)

#emissions_baltimore_type <- spread(emissions_baltimore_type, type,Emissions)
emissions_baltimore_type <- NEI %>%
  filter(fips == "24510")%>%
  group_by(year,type)%>%
  summarise(Emissions = sum(Emissions))


ggplot(data = emissions_baltimore_type) + 
  aes(x = year ,  y = Emissions, col = type) +
  geom_line()+
  labs( x = "Year", col="")
ggsave("plot3.png")
