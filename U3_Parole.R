# Loading the Dataset
read.csv("parole.csv")
str(parole)

table(parole$violator)

# Preparing the Dataset
parole$state = as.factor(parole$state)  # convert variable to factor for prediction
parole$crime = as.factor(parole$crime)

# Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Building a Logistic Regression Model
mod = glm(violator ~ ., data = train, family = "binomial")
summary(mod)

logit = -4.2411574 + 0.3869904 + 0.8867192 - 50*0.0001756 - 3*0.1238867 + 12*0.0802954 + 0.6837143
odds = exp(logit)  # odds ratio
p = 1 / (1 +exp(-logit))  # predicted probability of violation

# Evaluating the Model on the Testing Set
predictions  = predict(mod, newdata = test, type = "response")
max(predictions)  # maximum predicted probability of a violation

table(test$violator, predictions >= 0.5)
sensivity = 12 / (11 + 12)
specifity = 167 / (167 + 12)
accuracy = (12 + 167) / (167 + 12 + 11 + 12)

table(test$violator)
naive.accuracy = 179 / (179 + 23)  # accuracy of a simple model that predicts that every parolee is a non-violator

library(ROCR)
pred = prediction(predictions, test$violator)
auc.tmp = performance(pred, "auc")
#The probability the model can correctly differentiate between a randomly 
#selected parole violator and a randomly selected parole non-violator.
auc = as.numeric(auc.tmp@y.values)
