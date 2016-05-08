#A Logistic Regression Model
census = read.csv("census.csv")
library(caTools)
set.seed(2000)

spl = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, spl == TRUE)
test = subset(census, spl == FALSE)

censusLm = glm(over50k ~ ., data = train, family = "binomial")
summary(censusLm)

predcensusLm = predict(censusLm, newdata=test, type="response")
table(test$over50k, predcensusLm >= 0.5)
(9051 + 1888) / nrow(test)

table(train$over50k)
table(test$over50k)
9713 / nrow(test)

library(ROCR)
ROCRpred = prediction(predcensusLm, test$over50k)
ROCRPerf = performance(ROCRCARTpred, "tpr", "fpr")
plot(ROCRPerf)
as.numeric(performance(ROCRpred, "auc")@y.values)

#A CART Model
library(rpart)
library(rpart.plot)
censusCART = rpart(over50k ~ ., data=train, method="class")
prp(censusCART)

predCensusCART = predict(censusCART, newdata=test)
table(test$over50k, predCensusCART[,2] >= 0.5)
(9243 + 1596) / nrow(test)

ROCRCARTpred = prediction(predCensusCART[,2], test$over50k)
ROCRPerfCART = performance(ROCRCARTpred, "tpr", "fpr")
plot(ROCRPerfCART)
as.numeric(performance(ROCRCARTpred, "auc")@y.values)

#A Random Forest Model
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000),]
set.seed(1)
censusRandomForest = randomForest(over50k ~ ., data=trainSmall)
predCensusRandomForest = predict(censusRandomForest, newdata=test)
table(test$over50k, predCensusRandomForest)
(9586 + 1093) / nrow(test)

vu = varUsed(censusRandomForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return=TRUE)
dotchart(vusorted$x, names(censusRandomForest$forest$xlevels[vusorted$ix]))

#Selecting cp by Cross-Validation
library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl(method="cv", number=10)
cartGrid = expand.grid(.cp=seq(0.002, 0.1, 0.002))
train(over50k ~ ., data=train, method="rpart", trControl=numFolds, tuneGrid=cartGrid)

censusCARTCp = rpart(over50k ~ ., data=train, method="class", cp=0.002)
PredCensusCARTCp = predict(censusCARTCp, newdata=test)
table(test$over50k, PredCensusCARTCp[,2] >= 0.5)
(9178+1838) / nrow(test)
prp(censusCARTCp)