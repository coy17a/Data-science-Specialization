library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)
data <- segmentationOriginal
head(data)

index <- createDataPartition(data$Class, times =1, p =0.7, list = FALSE)
train1 <- data[index,]
test1 <-  data[-index,]
set.seed(125)

fit <- train(Class ~., data = train1, method = "rpart")

fancyRpartPlot(fit$finalModel)


library(pgmm)
data(olive)
olive = olive[,-1]

fit <- train(Area ~., data= olive, method = "rpart")
predict(fit, newdata = newdata)
newdata = as.data.frame(t(colMeans(olive)))


library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
fit4 <- train(chd~ age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")
p1<- predict(fit4,newdata=testSA)
p2<- predict(fit4,newdata=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd,p1)
missClass(trainSA$chd,p2)


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
fit5 <- train(y~., method="rf",data = vowel.train)
varImp(fit5)

