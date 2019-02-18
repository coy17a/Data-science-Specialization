library(readr)
library(dplyr)
library(Hmisc)
library(tidyr)
library(quantmod)
library(lubridate)

gdp <- read_csv("getdata_data_GDP.csv", col_names = TRUE,skip = 4)
edstats <- read_csv("getdata_data_EDSTATS_Country.csv",col_names = TRUE)
gdp <- gdp[!is.na(gdp[,"X2"]),]
gdp <- gdp[!is.na(gdp[,"X5"]),]
gdp[,"X5"] <-as.numeric(gsub(",","", gdp$X5))
##question 2 
mean(gdp$X5)
##question 3
r <- gdp%>% 
  filter ( grepl("^United",X4))
#question 4
gdp <- rename(gdp,"CountryCode"=X1)
combine <- left_join(gdp,edstats,by="CountryCode")

r2 <- combine  %>% 
 filter ( grepl("[Ff]iscal.*[Jj]une", `Special Notes`))

#question 5
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
sampleTimes <- as_date (sampleTimes)
x <- length(sampleTimes[year(sampleTimes) == 2012])
day <- length( sampleTimes[wday(sampleTimes) == 2 & year(sampleTimes) == 2012])
