table(Households$AfternoonPct >= 100)
table(Households$MorningPct >= 100)

min(Households$AvgDiscount[(Households$AvgSalesValue >= 150)])
min(Households$AvgSalesValue[(Households$AvgDiscount >= 25)])
table(Households$NumVisits >= 300)
148 / nrow(Households)

library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)

set.seed(200)
distances = dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers = hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
k = 10
KMC = kmeans(HouseholdsNorm, centers = k)
clusters = KMC$cluster
table(clusters)
KMC$centers

k = 5
set.seed(5000)
KMC = kmeans(HouseholdsNorm, centers = k)
clusters = KMC$cluster
table(clusters)
KMC$centers
