library(dplyr)
library(caret)
library(ggplot2)
library(readr)

training <- read_csv("pml-training.csv")
testing <- read_csv("pml-testing.csv")
str(training)
training$classe <- factor(training$classe)

training <- training %>%
  select(-c(1:7))%>%
select(matches("^roll|^pitch|^yaw|^total|^gyros|^accel|^magnet"), classe)
testing <- testing %>%
  select(-c(1:7))%>%
  select(matches("^roll|^pitch|^yaw|^total|^gyros|^accel|^magnet"))

train_df <- createDataPartition(training$classe, p =0.7, list = FALSE) 
train1 <- training[train_df,]
test1 <- training[-train_df,]

trControl <- trainControl(method = "cv", number = 5)
model1 <- train(classe ~. ,data = train1, method ="rpart", trControl =trControl )
p1 <- predict(model1, test1)  
confusionMatrix(p1,test1$classe)

model2 <- train(classe ~. ,data = train1, method ="rf", trControl =trControl,verbose = FALSE)
p2 <- predict(model2, test1)  
confusionMatrix(p2,test1$classe)

model3 <- train(classe ~. ,data = train1, method ="gbm", trControl =trControl,verbose = FALSE)
p3 <- predict(model3, test1)  
confusionMatrix(p3,test1$classe)

pfinal <- predict(model2, newdata = testing)
pfinal
