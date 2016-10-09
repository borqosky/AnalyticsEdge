energy <- read.csv("C:\\Users\\wborkows\\AppData\\Local\\Temp\\Rtmp67LUo3\\data52e877f416c4", header=TRUE)
View(energy)
 
i = which.max(energy$GenTotalRenewable)
energy$STATE[i]
energy$YEAR[i]

mean(energy$AllSourcesCO2[energy$presidential.results == 0], na.rm=TRUE)
mean(energy$AllSourcesCO2[energy$presidential.results == 1], na.rm=TRUE)

mean(energy$AllSourcesNOx[energy$presidential.results == 1], na.rm=TRUE) > mean(energy$AllSourcesNOx[energy$presidential.results == 0], na.rm=TRUE)

cor(energy$AllSourcesCO2, energy$EsalesCommercial, use="complete")
cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use="complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use="complete")

boxplot(EPriceTotal~STATE, data=energy)
table(mean(energy$EPriceTotal), energy$STATE)
tapply(energy$EPriceTotal, energy$STATE, mean)
tapply(energy$GenTotal, energy$STATE, mean)

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
mod = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, family = "binomial")
summary(mod)
pred = predict(mod, newdata=test, type="response")
table(test$GenSolarBinary, pred >= 0.5)

test0 = test[test$presidential.results == 0,]
pred0 = predict(mod, newdata=test0, type="response")
table(test0$GenSolarBinary, pred0 >= 0.5)

test1 = test[test$presidential.results == 1,]
pred1 = predict(mod, newdata=test1, type="response")
table(test1$GenSolarBinary, pred1 >= 0.5)

train.limited = train[c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")] 
test.limited = test[c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import")]

preproc.train = preProcess(train.limited)
train.norm = predict(preproc.train, train.limited)
preproc.test = preProcess(test.limited)
test.norm = predict(preproc.test, test.limited)

set.seed(144)
k = 2
KMC = kmeans(train.norm, centers = k, iter.max = 1000)
clusters = KMC$cluster


library(flexclust)
km.kcca = as.kcca(KMC, train.norm)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = test.norm)
table(clusterTest)

train1 = subset(train.limited, clusterTrain == 1)
train2 = subset(train.limited, clusterTrain == 2)
mean(train1$presidential.results)
mean(train2$presidential.results)

mean(train1$CumlRegulatory)
mean(train2$CumlRegulatory)
mean(train1$CumlFinancial)
mean(train2$CumlFinancial)

mean(train1$)
mean(train2$presidential.results)

mod1 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train1, family = "binomial")
summary(mod)
