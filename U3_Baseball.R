baseball = read.csv("baseball.csv")

#Limiting to Teams Making the Playoffs
str(baseball)
summary(baseball)
nrow(baseball)

length(table(baseball$Year))

baseball = subset(baseball, Playoffs == 1)

table(table(baseball$Year))

#Adding an Important Predictor
PlayoffTable = table(baseball$Year)
str(names(PlayoffTable))

PlayoffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

table(baseball$NumCompetitors == 8)

#Bivariate Models for Predicting World Series Winner
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

model = glm(baseball$WorldSeries ~ Year, data = baseball, family = "binomial")
summary(model)

#model = glm(baseball$WorldSeries ~ RS, data = baseball, family = "binomial")
#summary(model)

model = glm(baseball$WorldSeries ~ RA, data = baseball, family = "binomial")
summary(model)

#model = glm(baseball$WorldSeries ~ W, data = baseball, family = "binomial")
#summary(model)

#model = glm(baseball$WorldSeries ~ OBP, data = baseball, family = "binomial")
#summary(model)

#model = glm(baseball$WorldSeries ~ SLG, data = baseball, family = "binomial")
#summary(model)

#model = glm(baseball$WorldSeries ~ BA, data = baseball, family = "binomial")
#summary(model)

model = glm(baseball$WorldSeries ~ RankSeason, data = baseball, family = "binomial")
summary(model)

#model = glm(baseball$WorldSeries ~ OOBP, data = baseball, family = "binomial")
#summary(model)

#model = glm(baseball$WorldSeries ~ OSLG, data = baseball, family = "binomial")
#summary(model)

model = glm(baseball$WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
summary(model)

#model = glm(baseball$WorldSeries ~ League, data = baseball, family = "binomial")
#summary(model)

#Multivariate Models for Predicting World Series Winner
LogModel = glm(baseball$WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(LogModel)

cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

LogModel = glm(baseball$WorldSeries ~ Year + RA, data = baseball, family = "binomial")
summary(LogModel)
LogModel = glm(baseball$WorldSeries ~ Year + RankSeason, data = baseball, family = "binomial")
summary(LogModel)
LogModel = glm(baseball$WorldSeries ~ Year + NumCompetitors, data = baseball, family = "binomial")
summary(LogModel)
LogModel = glm(baseball$WorldSeries ~ RA + RankSeason, data = baseball, family = "binomial")
summary(LogModel)
LogModel = glm(baseball$WorldSeries ~ RA + NumCompetitors, data = baseball, family = "binomial")
summary(LogModel)
LogModel = glm(baseball$WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(LogModel)