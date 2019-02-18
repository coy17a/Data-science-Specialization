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
#plot 2
png("plot2.png")
barplot(emissions_baltimore$Emissions,names= emissions_baltimore$year, col= c("read"),
        main = "Total PM2.5 emissions from all Sources In Baltimore City",
        xlab= "Year", ylab = "PM2.5 emittet in Tons")
dev.off()


#emissions_baltimore_type <- spread(emissions_baltimore_type, type,Emissions)
emissions_baltimore_type <- NEI %>%
  filter(fips == "24510")%>%
  group_by(year,type)%>%
  summarise(Emissions = sum(Emissions))



ggplot(data = emissions_baltimore_type) + 
  aes(x = year ,  y = Emissions, col = type) +
  geom_line()+
  labs( x = "Year")
 ggsave("plot3.png")

##Coal Emissions

SCC <- SCC %>%
  select(SCC,EI.Sector)
coal_index <- SCC[grepl("[Cc]oal",SCC$EI.Sector),][1]

coal_emissions <- NEI %>%
  group_by(SCC,year) %>%
  summarise(Emissions = sum(Emissions))

coal_emissions <- coal_emissions[coal_emissions$SCC %in% coal_index$SCC,]
coal_emissions <- coal_emissions %>%
  group_by(year) %>%
  summarise(Emissions =sum(Emissions))

ggplot( data = coal_emissions) +
  aes(x= year, y = Emissions)+
  geom_col(stat = "identity", fill = "steelblue")+
 scale_x_continuous(breaks = c(1998,2002,2005,2008))+
  labs( x = "Year", y= "Emission(TON)",title = "Emissions from Coal combustion-related Source for USA")+
  theme_minimal()
ggsave("plot4.png")

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
##Motor Vehicle Emissions Baltimore vs Los Angeles

mv_comp_emissions <- NEI %>%
  filter(fips =="24510" | fips == "06037") %>%
  group_by(fips,year)%>%
    summarise(Emissions =sum(Emissions))
mv_comp_emissions$fips <- sapply(mv_comp_emissions$fips, function(x) ifelse(x =="24510","Baltimore","Los Angeles County"))

ggplot( data = mv_comp_emissions) +
  aes(x= year, y = Emissions, col = fips)+
  geom_line()+ geom_point()+
  labs( x = "Year", y= "Emission(TON)",title = "Emissions from Motor Vehicle combustion-related Source",col="")+
  theme_minimal()+
  theme(legend.position="top")
ggsave("plot6.png")
 

