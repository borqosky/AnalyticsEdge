#Preparing the Dataset
loans = read.csv("loans.csv")
table(loans$not.fully.paid)

not.fully.paid = 1533 / (8045 + 1533)

summary(loans)

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | 
                   is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | 
                   is.na(pub.rec))
table(missing$not.fully.paid)

library("mice")
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
mod = glm(not.fully.paid ~ ., data = train, family = "binomial")
summary(mod)

logit = -10 * -9.406e-03
exp(logit)  # O(A)/O(B)

#Prediction Models
predicted.risk = predict(mod, newdata = test, type = "response")
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk >= 0.5)
model.accuracy = (2400 + 3) / (2400 + 13 + 457 + 3)
table(test$not.fully.paid)
naive.accuracy = 2413 / (2413 + 460)

library(ROCR)
pred = prediction(predicted.risk, test$not.fully.paid)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

bivariate = glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(bivariate)

#A "Smart Baseline"
bivariate.pred = predict(bivariate, newdata = test, type = "response")
max(bivariate.pred)
table(test$not.fully.paid, bivariate.pred >= 0.5)

pred = prediction(bivariate.pred, test$not.fully.paid)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

#Computing the Profitability of an Investment
#A Simple Investment Strategy
test$profit = exp(test$int.rate * 3) - 1
test$profit[test$not.fully.paid == 1] = -1
10 * max(test$profit)  # max profit of investment 10$

#An Investment Strategy Based on Risk
highInterest = subset(test, int.rate >= 0.15)
profit.avg = mean(highInterest$profit)
table(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing = FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)