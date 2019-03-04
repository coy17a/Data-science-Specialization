library(ElemStatLearn)
library(caret)
library(rpart)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

fit1 <- train(y ~., method = "rf",data=vowel.train)
fit2 <- train(y ~., method ="gbm", data = vowel.train)

p1 <- predict(fit1, vowel.test)
p2 <- predict(fit2, vowel.test)

a1 <- mean(p1 == vowel.test$y)
confusionMatrix(p1,vowel.test$y)
a2 <- mean(p2 == vowel.test$y)

confusionMatrix(p1,p2)['Accuracy']

library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fit3 <- train(diagnosis ~., method="rf", data= training)
fit4 <- train(diagnosis ~., method="gbm", data= training,verbose =FALSE)
fit5 <- train(diagnosis ~., method="lda", data= training)
p3 <- predict(fit3,testing)
a3 <- mean(p3 == testing$diagnosis)

p4 <- predict(fit4,testing)
a4 <- mean(p4 == testing$diagnosis)

p5 <- predict(fit5,testing)
a6 <- mean(p5 == testing$diagnosis)

newP <- data.frame(p3,p4,p5,diagnosis= testing$diagnosis)
combfit <- train(diagnosis ~.,method ="rf",data=newP)
acomb <- mean(predict(combfit,testing) == testing$diagnosis)

##question 3
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
fit6 <- train(CompressiveStrength ~., method="lasso",data=training)
summary(fit6)
?plot.enet

##question 4

library(lubridate) # For year() function below
library(forecast)
dat = read.csv("~/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
model2 <- bats(tstrain)
##question 5
library(e1071)
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
model <- svm(CompressiveStrength~., data=training)
pl <- predict(model,testing)
RMSE(pl,testing$CompressiveStrength)



