library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(readxl)
library(DT)


wasaData18 <- read_excel("wasaData.xlsx", sheet = "2018")
wasaData17 <- read_excel("wasaData.xlsx", sheet = "2017")
wasaData16 <- read_excel("wasaData.xlsx", sheet = "2016")
wasaData15 <- read_excel("wasaData.xlsx", sheet = "2015")

wasaData18 <- rename(wasaData18, Division ="20")
cleandata <- function(x,y) {
  x %>%
  rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
  separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
    separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
    separate(Place,into = c("Position","TotalP"), sep = "/")%>%
    fill(Swim, T1,Bike,T2,Run)%>%
    select(-c("Event","Race #","ToalAge","ToaGender","TotalP"))%>%
    mutate_at(c(5:6,1),as.numeric)%>%
    filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
    mutate(category=as.factor(category))%>%
    mutate(gender = ifelse(str_detect(category,"F"),"F","M"))%>%
    mutate(year = as.factor(y))%>%
    mutate(category = str_replace(category,"[A-Z]",""))%>%
    filter(!is.na(Time))
}

wasaData18c <- cleandata(wasaData18,2018)
wasaData17c <- cleandata(wasaData17,2017)
wasaData16c <- cleandata(wasaData16,2016)
wasaData15c <- cleandata(wasaData15,2015)

wasaData <- bind_rows(wasaData18c,wasaData17c,wasaData16c,wasaData15c)
wasaData$year <- as.factor(wasaData$year) 

ggplot(wasaData, aes(x=Time,fill=year))+
  geom_histogram(alpha=0.5)

wasafilter <- wasaData18c %>%
  filter(gender == "M")%>%
  group_by(category)%>%
  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
  gather(key="sport",value = "averageTime", 2:4)


wasafilter$averageTime<- duration(hour = hour(wasafilter$averageTime), minute = minute(wasafilter$averageTime), 
                          second = second(wasafilter$averageTime))

wasafilter$averageTime <- as.numeric(wasafilter$averageTime)
yLabels <- function(x)
{
  x <- seconds_to_period(x)
  paste(hour(x),minute(x),second(x), sep = ':')
}
labels <- c(0,2500,5000,7500,10000,12500)
labelst <- sapply(labels,yLabels)


wasafilter <- wasaData18c %>%
  filter(gender == "F")%>%
  group_by(category)%>%
  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
  gather(key="sport",value = "averageTime", 2:4)

  wasafilter <- wasafilter %>%
  mutate(averageTime = duration(hour = hour(wasafilter$averageTime), minute = minute(wasafilter$averageTime),second = second(wasafilter$averageTime)))%>%
  mutate(averageTime = as.numeric(averageTime))


ggplot(wasafilter, aes(x=category, y=averageTime,fill=sport))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = labelst)+
  coord_flip()
  
##Filter by gender
filter1 <- "F"
wasaData18FilterG <- wasaData18c %>%
  filter(str_detect(category, filter1))

##Filter by Category
filter2 <- "M3034"
wasaData18FilterC <- wasaData18c %>%
  filter(str_detect(category, filter2))
paste(filter1,filter2,sep = "")
## Plot Position
ggplot(wasaData18FilterC, aes(x=AgePosition, y = Time))+
  geom_point()
##Print Table top 5
datatable(data = wasaData18FilterC[,], 
              options = list(pageLength = 5), 
              rownames = FALSE)
