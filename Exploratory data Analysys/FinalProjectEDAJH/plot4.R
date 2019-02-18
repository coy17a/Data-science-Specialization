library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)

##Coal Emissions
coal_emissions <- NEI %>%
  group_by(SCC,year) %>%
  summarise(Emissions = sum(Emissions))
##Filter the data fro Coal using Regular Expressions
SCC <- SCC %>%
  select(SCC,EI.Sector)
coal_index <- SCC[grepl("[Cc]oal",SCC$EI.Sector),][1]

coal_emissions <- coal_emissions[coal_emissions$SCC %in% coal_index$SCC,]
coal_emissions <- coal_emissions %>%
  group_by(year) %>%
  summarise(Emissions =sum(Emissions))
##plot 
ggplot( data = coal_emissions) +
  aes(x= year, y = Emissions)+
  geom_col(stat = "identity", fill = "steelblue")+
  scale_x_continuous(breaks = c(1998,2002,2005,2008))+
  labs( x = "Year", y= "Emission(TON)",title = "Emissions from Coal combustion-related Source for USA")+
  theme_minimal()
ggsave("plot4.png")