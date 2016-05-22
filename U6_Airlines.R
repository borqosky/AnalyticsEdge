airlines = read.csv("airlines.csv")

summary(airlines)

library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

str(airlinesNorm)
distance = dist(airlinesNorm, method = "euclidean")
clusterAirlines = hclust(distance, method = "ward.D")
plot(clusterAirlines)
rect.hclust(clusterAirlines, k=6, border = "red")

airlineClusters = cutree(clusterAirlines, k = 5)
table(airlineClusters)

tapply(airlines$Balance, airlineClusters, mean)
tapply(airlines$QualMiles, airlineClusters, mean)
tapply(airlines$BonusMiles, airlineClusters, mean)
tapply(airlines$BonusTrans, airlineClusters, mean)
tapply(airlines$FlightMiles, airlineClusters, mean)
tapply(airlines$FlightTrans, airlineClusters, mean)
tapply(airlines$DaysSinceEnroll, airlineClusters, mean)

lapply(split(airlines, airlineClusters), colMeans)

set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
table(KMC$cluster)

#lapply(split(airlines, KMC$cluster), mean)
tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)
