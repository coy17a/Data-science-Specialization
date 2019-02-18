library(datasets)
library(dplyr)
library(ggplot2)
data <- ToothGrowth
glimpse(data)
str(data)
head(data)


ggplot(data,aes(factor(dose),y=len,fill=supp))+
  geom_col()+
  facet_wrap(~supp)+
  theme_bw()+
  labs(x="Dose in mm",y="Tooth Lenght",fill="Suppy Method")

dose_0.5 <- data %>%
  filter(dose == 0.5)
dose_1 <- data %>%
  filter(dose == 1)
dose_2 <- data %>%
  filter(dose == 2)

 t.test(dose_0.5$len,dose_1$len,paired = FALSE)
t.test(dose_0.5$len,dose_2$len,paired = FALSE)
t.test(dose_1$len,dose_2$len,paired = FALSE)



vc_0.5 <- data %>%
  filter(dose == 0.5, supp =="VC")
vc_1 <- data %>%
  filter(dose == 1, supp =="VC")
vc_2 <- data %>%
  filter(dose == 2, supp =="VC")
oj_0.5 <- data %>%
  filter(dose == 0.5, supp =="OJ")
oj_1 <- data %>%
  filter(dose == 1, supp =="OJ")
oj_2 <- data %>%
  filter(dose == 2, supp =="OJ")
t.test(vc_0.5$len,oj_0.5$len,paired = FALSE)
t.test(vc_1$len,oj_1$len,paired = FALSE)
t.test(vc_2$len,oj_2$len,paired = FALSE)
