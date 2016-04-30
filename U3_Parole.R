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
