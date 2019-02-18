library(readr)
library(dplyr)
library(Hmisc)

gdp <- read_csv("getdata_data_GDP.csv", col_names = TRUE,skip = 4)
edstats <- read_csv("getdata_data_EDSTATS_Country.csv", col_names = TRUE)
names(gdp)    
names(edstats)
gdp <- gdp[!is.na(gdp[,"X2"]),]

combine <- merge(edstats,gdp,by.x="CountryCode",by.y= "X1",all =FALSE)
combine[,"X5"] <- as.numeric( gsub(",","",combine[,"X5"]))
combine <- combine[order(combine["X5"]),]
combine$rank <- rank(-combine$X5)
combine %>%
filter(`Income Group`== "High income: OECD")%>%
  summarise(mean(rank))
combine %>%
  filter(`Income Group`== "High income: nonOECD")%>%
  summarise(mean(rank))

combine$rankG <- cut2(combine$rank,g=5)
table(combine$`Income Group`,combine$rankG)

 test <- combine %>% 
  filter ( grepl("^United",`Long Name`))
