# Шаг 0. Прочитаем данные
#  credit.01 <- read.table("Credit_ru_1.csv", header=T, sep=";")

wine <- read.table("data.csv", header=T, sep=";")
wine[is.na(wine[,10]),10]<- -9999

set.seed(12345)
wine<- wine[,-14]
r.1 <- runif(50001, 0, 1)


wine.train <- wine[ r.1 >= 0.25, ]
wine.test  <- wine[ r.1 < 0.25,  ]
names(wine)
View(wine)
x <- wine.train[, 1:16]

y <- wine.train[, 17]
y.1 <- as.factor(y)


library(randomForest)


table(y)

ntree.1 <- 500
set.seed(3217)

nodesize.1 <- 10

keep.forest.1<-TRUE
length(x)
length(y.1)

class(wine[,17])

#  names(wine)
rf.res <- randomForest(x, y=y.1, ntree=ntree.1, mtry=floor(sqrt(ncol(wine.train))),
                       replace=FALSE, nodesize = nodesize.1,
                       importance=TRUE, localImp=FALSE,
                       proximity=FALSE, norm.votes=TRUE, do.trace=ntree.1/10,
                       keep.forest=keep.forest.1, corr.bias=FALSE, keep.inbag=FALSE)



wine.predict.1 <- predict(rf.res, wine.train)
table(y, wine.predict.1)
wine.predict.2 <- predict(rf.res, wine.test[,-17])
table(wine.test[,-17], wine.predict.2)

zzz.predict <- predict(rf.res, wine.train)
table(y, zzz.predict)

z.2 <-predict(rf.res, wine.test[,-17])
table(wine.test[,17], z.2)

(1-sum(diag(table(y,wine.train[,17])/nrow(wine.train))*100))

library(rpart.plot)


rpart.plot(wine.res, type=2, extra = 2)


predict(wine.res, wine.train[ , -18], type="class")



table(wine.train[ , 18], 
      predict(wine.res, wine.train[ , -18], type="class"))

table(wine.test[ , 18], 
      predict(wine.res, wine.test[ , -18], type="class"))



