read.csv("songs.csv")

# Understanding the data
table(Songs$year >= 2010)

MichaelJackson = subset(Songs, Songs$artistname == "Michael Jackson")
nrow(MichaelJackson)

MichaelJackson[c("songtitle", "Top10")]

table(Songs$timesignature)
which.max(table(Songs$timesignature))

Songs$songtitle[which.max(Songs$tempo)]

# Creating prediction model
SongsTrain = subset(Songs, year <= 2009)
SongsTest = subset(Songs, year == 2010)
nrow(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[!(names(SongsTrain)) %in% nonvars]
SongsTest = SongsTest[!names(SongsTest) %in% nonvars]

SongsLog1 = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

#Beware of Multicollinearity Issues!
cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data = SongsTrain, family = binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data = SongsTrain, family = binomial)
summary(SongsLog3)

#Validating Our Model
testPredict = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, TestPredicion > 0.45) # confusion matrix
acc = (19 + 309) / (19 + 309 + 5 + 40) # accuracy model

table(SongsTest$Top10) # naive approach 
acc2 = 314 / (314+59) # baseline accuracy

table(SongsTest$Top10, TestPredicion > 0.45) # number of guesses
sensivity = 19 / (40 + 19)
specifity = 309 / (5 + 309)

