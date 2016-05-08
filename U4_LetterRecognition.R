letters = read.csv("letters.csv")

#Predicting B or not B
str(letters)
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, spl == TRUE)
test = subset(letters, spl == FALSE)

table(test$isB) / nrow(test)

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ .-letter, data=train, method="class")
prp(CARTb)

predCARTb = predict(CARTb, newdata=test, type="class")
table(test$isB, predCARTb)
predCARTb.accuracy = (1118 + 340) / (1118 + 340 + 57 + 43)

library(randomForest)
set.seed(1000)
RandomForestb = randomForest(isB ~ .-letter, data=train)
predRandomForestb = predict(RandomForestb, newdata=test)
table(test$isB, predRandomForestb)
predRandomForestb.accuracy = (1165 + 374) / nrow(test)

#Predicting the letters A, B, P, R
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl2 = sample.split(letters$letter, SplitRatio=0.5)
train2 = subset(letters, spl2 == TRUE)
test2 = subset(letters, spl2 == FALSE)

table(test2$letter)
baseline2.accuracy = 401 / nrow(test)

CARTabpr = rpart(letter ~ . - isB, data=train2, method="class")
predCARTabpr = predict(CARTabpr, newdata=test2, type="class")
table(test2$letter, predCARTabpr)
predCARTabpr.accuracy = (348 + 318 + 363 + 340) / nrow(test2)

set.seed(1000)
RandomForestabpr = randomForest(letter ~ . - isB, data=train2)
predRandomForestabpr = predict(RandomForestabpr, newdata=test2)
table(test2$letter, predRandomForestabpr)
predRandomForestabpr.accuracy = (390 + 380 + 392+ 369) /nrow(test2)
