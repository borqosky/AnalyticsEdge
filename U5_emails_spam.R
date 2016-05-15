#Loading the Dataset
emails = read.csv("emails.csv", stringsAsFactors = FALSE)

str(emails)
table(emails$spam)

emails$text[1]

max(nchar(emails$text))
which.min(nchar(emails$text))
emails$text[1992]

#Preparing the Corpus
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

spdtm = removeSparseTerms(dtm, sparse = 0.95)
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse)) 
emailsSparse$spam = emails$spam

hams = subset(emailsSparse, spam == 0)
length(hams[colSums(hams) >= 5000])

sort(colSums(subset(emailsSparse, spam == 1)))

#Building machine learning models
library(caTools)
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = "binomial")

library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., method = "class", data = train)

library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train) 

predTrainLog = predict(spamLog, type = "response")
predTrainCART = predict(spamCART)[,2] 
predTrainRF = predict(spamRF, type = "prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog  > 0.99999)
nrow(train) - 3046 - 954

summary(spamLog)

prp(spamCART)

table(train$spam, predSpamLog >= 0.5)
(3052 + 954) / nrow(train)

library(ROCR)
pred = prediction(predTrainLog, train$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)
	
table(train$spam, predTrainCART >= 0.5)
(2885 + 894) / nrow(train)
pred = prediction(predTrainCART, train$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

table(train$spam, predTrainRF >= 0.5)
(3013 + 914) / nrow(train)
pred = prediction(predTrainRF, train$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

#Evaluating on the Test Set
predTestLog = predict(spamLog, newdata = test, type = "response")
predTestCART = predict(spamCART, newdata = test)[,2] 
predTestRF = predict(spamRF, newdata = test, type = "prob")[,2]

table(test$spam, predTestLog >= 0.5)
(1257 + 376) / nrow(test)
pred = prediction(predTestLog, test$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

table(test$spam, predTestCART >= 0.5)
(1228 + 386) / nrow(test)
pred = prediction(predTestCART, test$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

table(test$spam, predTestRF >= 0.5)
(1290 + 386) / nrow(test)
pred = prediction(predTestRF, test$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

wordCount = rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)
set.seed(123)
spam2CART = rpart(spam ~ ., method = "class", data = train2)
set.seed(123)
spam2RF = randomForest(spam ~ ., data = train2) 
prp(spam2CART)

predTest2CART = predict(spam2CART, newdata = test2)[,2] 
predTest2RF = predict(spam2RF, newdata = test2, type = "prob")[,2]

table(test2$spam, predTest2CART >= 0.5)
(1214 + 385) / nrow(test2)
pred = prediction(predTest2CART, test2$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)

table(test2$spam, predTest2RF >= 0.5)
(1296 + 383) / nrow(test2)
pred = prediction(predTest2RF, test2$spam)
auc.tmp = performance(pred, "auc")
auc = as.numeric(auc.tmp@y.values)
