read.csv("gerber.csv")
str(gerber)

#Exploration and Logistic Regression
table(gerber$voting) / nrow(gerber)

summary(subset(gerber, voting == 1))

modelLog = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = "binomial")
summary(modelLog)

predLog = predict(modelLog, type="response")
table(gerber$voting, predLog >= 0.3)
acc1 = (51966 + 134513) / nrow(gerber)

table(gerber$voting, predLog >= 0.5)
acc2 = 235388 / nrow(gerber)

table(gerber$voting)
baseline = 235388 / (235388 + 108696)
library(ROCR)
ROCRPred = prediction(predLog, gerber$voting)
auc = as.numeric(performance(ROCRPred, "auc")@y.values)

#Trees
library(rpart)
library(rpart.plot)
CARTModel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTModel)

CARTModel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTModel2)

CARTModel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTModel3)

#Interaction Terms
CARTModel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTModel4, digits=6)
abs(0.296638 - 0.34)

CARTModel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTModel5, digits=6)
women = 0.290456 - 0.334176
man = 0.302795 - 0.345818

modelLog2 = glm(voting ~ sex + control, data = gerber, family = "binomial")
summary(modelLog2)

Possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predictPossibilities = predict(modelLog2, newdata=Possibilities, type="response")
abs(predictPossibilities[4] - 0.290456)

modelLog3 = glm(voting ~ sex + control + sex:control, data = gerber, family = "binomial")
summary(modelLog3)

predictPossibilities2 = predict(modelLog3, newdata=Possibilities, type="response")
abs(predictPossibilities[4] - 0.290456)
