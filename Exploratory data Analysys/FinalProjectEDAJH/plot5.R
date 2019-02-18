library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)

##Motor Vehicle Emissions for Baltimore

mv_index <- SCC[grepl("[Vv]ehicles",SCC$EI.Sector),][1]

mv_emissions <- NEI %>%
  filter(fips == "24510")%>%
  group_by(SCC,year) %>%
  summarise(Emissions = sum(Emissions))

mv_emissions <- mv_emissions[mv_emissions$SCC %in% mv_index$SCC,]
mv_emissions <- mv_emissions %>%
  group_by(year) %>%
  summarise(Emissions =sum(Emissions))

ggplot( data = mv_emissions) +
  aes(x= year, y = Emissions)+
  geom_col(stat = "identity", fill = "steelblue")+
  scale_x_continuous(breaks = c(1998,2002,2005,2008))+
  labs( x = "Year", y= "Emission(TON)",title = "Emissions from Motor Vehicle combustion-related Source for Baltimore")+
  theme_minimal()
ggsave("plot5.png")