library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(GGally)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

str(mixtures)
training2 <-training
training2$CompressiveStrength <- cut2(training2$CompressiveStrength, g =3)
ggpairs(data = training2, aes(color=CompressiveStrength))


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
adData <- adData %>%
  select(diagnosis,starts_with("IL"))%>%
  mutate(diagnosis = factor(diagnosis))

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

pcat <- preProcess(training[-1],method = "pca",thresh = 0.9)

model <- train(diagnosis ~ . , data = training, method = "glm")
p <- predict(model,testing)
confusionMatrix(p,testing$diagnosis)$overall[1]

modelPCA <- train(diagnosis ~., data =training, method = "glm", preProcess="pca", trControl=trainControl(preProcOptions = list(thresh=0.8)))
p2 <- predict(modelPCA,testing)
confusionMatrix(p2,testing$diagnosis)$overall[1]
