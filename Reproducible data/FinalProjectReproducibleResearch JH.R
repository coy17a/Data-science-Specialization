library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read_csv("repdata_data_StormData.csv.bz2")
names(data)
summary(data)

#---------------------------------------------------------------
#Harmful Events

by_event <- data %>%
  select(EVTYPE, FATALITIES, INJURIES,STATE)%>%
  filter(FATALITIES > 0 | INJURIES >0 )%>%
  group_by(EVTYPE)%>%
  summarize(Fatalities = sum(FATALITIES),Injuries=sum(INJURIES))%>%
  mutate( TOTAL = Fatalities + Injuries)%>%
  arrange(desc(TOTAL))%>%
  #Events filter with more than 1000 Incidents
  filter(TOTAL > 1000) %>%
  gather(key="Harm",value="Amount", 2:3)


ggplot(by_event)+
  aes(x=reorder(EVTYPE,TOTAL),y= Amount, fill= Harm)+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y="Total Harmful Events", x = "",title="Weather Events Most Harmful to Population USA(1950-2011)")+
  theme_minimal()


#---------------------------------------------------------------
#Property Damage
valid_ex<- c("M","m","B","H","h","K","k")
pdamage <- data %>%
  select(EVTYPE,PROPDMG,PROPDMGEXP)%>%
  filter(!is.na(PROPDMGEXP))%>%
  group_by(PROPDMGEXP,EVTYPE)%>%
  summarise(total = sum(PROPDMG))%>%
  filter(PROPDMGEXP %in% valid_ex)

fromMtoB <- pdamage$PROPDMGEXP == "M" | pdamage$PROPDMGEXP == "m"
fromktoB <- pdamage$PROPDMGEXP == "K" | pdamage$PROPDMGEXP == "k"
fromhtoB <- pdamage$PROPDMGEXP == "H" | pdamage$PROPDMGEXP == "h"

pdamage$total[fromMtoB] <- pdamage$total[fromMtoB]/1000
pdamage$total[fromktoB] <- pdamage$total[fromktoB]/1000000
pdamage$total[fromhtoB] <- pdamage$total[fromhtoB]/1000000000

pdamageTotal <- pdamage %>%
  group_by(EVTYPE) %>%
  summarize(totalProp_damage = sum(total))%>%
  arrange(desc(totalProp_damage))

#---------------------------------------------------------------
#cropDamage
cdamage2 <- data %>%
  select(EVTYPE,CROPDMG,CROPDMGEXP)%>%
  selec(!is.na(CROPDMGEXP))
  
cdamage <- data %>%
  select(EVTYPE,CROPDMG,CROPDMGEXP)%>%
  filter(CROPDMG >0)%>%
  group_by(EVTYPE)%>%
  summarise(totalCrop_damage = sum(CROPDMG)/1000000)%>%
  arrange(desc(totalCrop_damage))

compilation <- left_join(pdamageTotal,cdamage,by="EVTYPE") %>%
  #replace Na with 0
  mutate_all(funs(replace(.,is.na(.),0))) %>%
  mutate(Total = totalCrop_damage + totalProp_damage)
#Events filter with more than 2 billion in damage
ggplot(filter(compilation,compilation$Total > 2))+
  aes(x=reorder(EVTYPE,-Total),y= Total)+
  geom_bar(stat = "identity",fill="brown")+
  coord_flip()+
  labs(y="Total Cost in damage(Billions)", x = "",title="Weather Events Most Costly in USA(1950-2011)")+
  theme_minimal()
