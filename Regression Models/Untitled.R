library(dplyr)
library(ggplot2) 
library(GGally)
library(caret)

#“Is an automatic or manual transmission better for MPG”
# "Quantify the MPG difference between automatic and manual transmissions"
?mtcars
str(mtcars)
pairs(mtcars)
data <- mtcars %>%
  mutate_at(c("cyl","gear","carb","am","vs"), as.factor)
str(data)

levels(data$am) <- list(automatic="0", manual="1")
str(data)

ggpairs(mtcars)
ggplot(data = mtcars, aes(y =am, x = mpg))+
  geom_point()


ggplot(data = data)+
  aes(x = am, y = mpg,fill=am)+
  geom_boxplot()

automatic <- data %>%
  filter(am =="automatic" )
manual<- data %>%
  filter(am =="manual")

?t.test
t.test(automatic$mpg,manual$mpg,paired = FALSE)

fit <- lm(mpg ~ am, data = data)
summary(fit)

fit2 <- lm(mpg ~ disp + hp + drat + am + wt  , data= data)
summary(fit2)

fit3 <- lm(mpg ~ disp + hp + drat + am + wt+ cyl, data= data)
summary(fit3)

anova(fit,fit2,fit3)

qplot(predict(fit2), resid(fit2))


