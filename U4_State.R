#Linear Regression Models
statedata = read.csv("statedata.csv")
stateLm = lm(Life.Exp ~ ., data = statedata)
summary(stateLm)

predState = predict(stateLm)
sse = sum((predState - statedata$Life.Exp)^2)

stateLm2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(stateLm2)
predState2 = predict(stateLm2)
sse2 = sum((predState2 - statedata$Life.Exp)^2)

#CART models
library(rpart)
library(rpart.plot)
stateCART = rpart(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
prp(stateCART)

predStateCart = predict(stateCART)
sse3 = sum((predStateCart - statedata$Life.Exp)^2)

stateCART2 = rpart(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata, minbucket=5)
prp(stateCART2)
predStateCART2 = predict(stateCART2)
sse4 = sum((predStateCART2 - statedata$Life.Exp)^2)

stateCART3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
prp(stateCART3)
predStateCART3 = predict(stateCART3)
sse4 = sum((predStateCART3 - statedata$Life.Exp)^2)

#Cross-validation
library(caret)
library(e1071)
set.seed(111)

tr.control = trainControl(method = "cv", number=10)
cp.grid  = expand.grid(.cp = (0:50)*0.01)
tr = train(Life.Exp ~ ., data=statedata, method="rpart", trControl=tr.control, tuneGrid=cp.grid)
stateCART4 = rpart(Life.Exp ~ ., data=statedata, cp=0.12)
prp(stateCART4)
predStateCART4 = predict(stateCART4)
sse4 = sum((predStateCART4 - statedata$Life.Exp)^2)

set.seed(111)
tr = train(Life.Exp ~ Area, data=statedata, method="rpart", trControl=tr.control, tuneGrid=cp.grid)
stateCART5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
prp(stateCART5)
predStateCART5 = predict(stateCART5)
sse5 = sum((predStateCART5 - statedata$Life.Exp)^2)
