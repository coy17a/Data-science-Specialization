library(dplyr)
library(readxl)
library(lubridate)
library(plotly)
library(tidyr)
wasaData <- read_excel("~/Dropbox/usbBackup/WebCoy/data/DataScience Projects/EDA Wasa Triathlon/wasaData.xlsx",sheet = "2018")
wasaData$Swim <- ymd_hms(wasaData$Swim)

wasaM <- wasaData %>%
  filter(grepl("M",`20`) )
wasaF <- wasaData %>%
  filter(grepl("F",`20`) )

plot_ly(alpha = 0.5)%>%
  add_histogram(x = wasaM$Time, name ="Male") %>%
  add_histogram(x = wasaF$Time, name = "Female")%>%
  layout(barmode ="overlay", title = "Distribution Time Wasa triathlon",
                  xaxis= list(title = "Time(h)"), yaxis = list(title = "Athlete"))
