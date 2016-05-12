wiki = read.csv("wiki.csv")

str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

sparseAdded = removeSparseTerms(dtmAdded, 0.997)

wordsAdded = as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))


corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal


library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)
table(test$Vandal)
618 / nrow(test)

library(rpart)
library(rpart.plot)
stateCART = rpart(Vandal ~ ., data = train, method = "class")
predCART = predict(stateCART, newdata = test, type="class")
table(test$Vandal, predCART)
(618 + 12) /nrow(test)

prp(stateCART)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl == TRUE)
wikiTest2 = subset(wikiWords2, spl == FALSE)

stateCART2 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")
predCART2 = predict(stateCART2, newdata = wikiTest2, type="class")
prp(stateCART2)
table(wikiTest2$Vandal, predCART2)
(618 + 12) / nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2)

wikiTrain3 = subset(wikiWords2, spl == TRUE)
wikiTest3 = subset(wikiWords2, spl == FALSE)

stateCART3 = rpart(Vandal ~ ., data = wikiTrain3, method = "class")
predCART3 = predict(stateCART3, newdata = wikiTest3, type="class")
prp(stateCART3)
table(wikiTest3$Vandal, predCART3)
(514 + 248) / nrow(wikiTest3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl == TRUE)
wikiTest4 = subset(wikiWords3, spl == FALSE)

stateCART4 = rpart(Vandal ~ ., data = wikiTrain4, method = "class")
predCART4 = predict(stateCART4, newdata = wikiTest4, type="class")
prp(stateCART4)
table(wikiTest4$Vandal, predCART4)
(595 + 241) / nrow(wikiTest4)
