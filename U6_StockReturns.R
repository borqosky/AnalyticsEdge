stocks = read.csv("StocksCluster.csv")

str(stocks)

mean(stocks$PositiveDec)

cor(stocks)

stocks.no.dec = stocks
stocks.no.dec$PositiveDec = NULL
which.max(colMeans(stocks.no.dec))
which.min(colMeans(stocks.no.dec))

set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
summary(StocksModel)
table(stocksTrain$PositiveDec, predict(StocksModel, type = "response") >= 0.5)
(990 + 3640) / nrow(stocksTrain)

PredictTest = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, PredictTest >= 0.5)
(417 + 1553) / nrow(stocksTest)

table(stocksTest$PositiveDec)
1897 / nrow(stocksTest)

# Clustering the stocks
limitedtrain = stocksTrain
limitedtrain$PositiveDec = NULL
limitedtest = stocksTest
limitedtest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedtrain)
normTrain = predict(preproc, limitedtrain)
normTest = predict(preproc, limitedtest)

summary(normTrain)
summary(normTest)

set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTest1$PositiveDec)
mean(stocksTest2$PositiveDec)
mean(stocksTest3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
(30 + 774) / nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
(388 + 757) / nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)
(49 + 13) / nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467 + 1544) / length(AllPredictions)