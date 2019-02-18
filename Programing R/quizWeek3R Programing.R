library(datasets)
data(iris)
iris
virl <- iris[iris$Species == "virginica",]
round(mean(virl$Sepal.Length))
apply(iris[,1:4],2,mean)
#------------------------------------------------
data(mtcars)
mtcars

with(mtcars, tapply(mpg, cyl, mean))
tapply(mtcars$mpg, mtcars$cyl, mean)
t <- sapply(split(mtcars$hp, mtcars$cyl), mean)
round(t[1]-t[3])

debug(ls)
