#Loading the Data
trials = read.csv("trials.csv")

max(nchar(trials$abstract))
sum(nchar(trials$abstract) == 0)
trials$title[which.min(nchar(trials$title))]

#Preparing the Corpus
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

#Preparing the Corpus
which.max(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

#Building a Model
library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trial)
730 / nrow(train)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)

predTrain = predict(trialCART)
summary(predTrain)
table(train$trial, predTrain[,2] >= 0.5)

(631 + 441) / nrow(train)
441 / (131 + 441)
631 / (631 + 99)

predTest = predict(trialCART, newdata = test, method = "class")
table(test$trial, predTest[,2] >= 0.5)
(261 + 162) / nrow(test)

library(ROCR)
ROCRpred = prediction(predTest[,2], test$trial)
ROCRPerf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRPerf)
as.numeric(performance(ROCRpred, "auc")@y.values)

#DECISION-MAKER TRADEOFFS
