library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC[] <- lapply(SCC, as.character)
mv_index <- SCC[grepl("[Vv]ehicles",SCC$EI.Sector),][1]

mv_comp_emissions <- NEI %>%
  filter(fips =="24510" | fips == "06037") %>%
  group_by(fips,year)%>%
  summarise(Emissions =sum(Emissions))
mv_comp_emissions$fips <- sapply(mv_comp_emissions$fips, function(x) ifelse(x =="24510","Baltimore","Los Angeles County"))

mv_comp_emissions <- mv_comp_emissions[mv_comp_emissions$SCC %in% mv_index$SCC,]
mv_comp_emissions <- mv_comp_emissions %>%
  group_by(year) %>%
  summarise(Emissions =sum(Emissions))

ggplot( data = mv_comp_emissions) +
  aes(x= year, y = Emissions, col = fips)+
  geom_line()+ geom_point()+
  labs( x = "Year", y= "Emission(TON)",title = "Emissions from Motor Vehicle combustion-related Source",col="")+
  theme_minimal()+
  theme(legend.position="top")
ggsave("plot6.png")
