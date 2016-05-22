dailykos = read.csv("dailykos.csv")

str(dailykos)

dailykosMatrix = as.matrix(dailykos)

#dailykosVector = as.vector(dailykosMatrix)
#str(dailykosVector)

distances = dist(dailykosMatrix, method = "euclidean")
kosHierClust = hclust(distances, method = "ward.D")
plot(kosHierClust)

rect.hclust(kosHierClust, k=7, border = "red")
dailykosClusters = cutree(kosHierClust, k=7)

dailykosCluster1 = subset(dailykos, dailykosClusters == 1)
dailykosCluster2 = subset(dailykos, dailykosClusters == 2)
dailykosCluster3 = subset(dailykos, dailykosClusters == 3)
dailykosCluster4 = subset(dailykos, dailykosClusters == 4)
dailykosCluster5 = subset(dailykos, dailykosClusters == 5)
dailykosCluster6 = subset(dailykos, dailykosClusters == 6)
dailykosCluster7 = subset(dailykos, dailykosClusters == 7)

table(dailykosClusters)

tail(sort(colMeans(dailykosCluster1)))
tail(sort(colMeans(dailykosCluster2)))
tail(sort(colMeans(dailykosCluster3)))
tail(sort(colMeans(dailykosCluster4)))
tail(sort(colMeans(dailykosCluster5)))
tail(sort(colMeans(dailykosCluster6)))
tail(sort(colMeans(dailykosCluster7)))

set.seed(1000)
k = 7
KMC = kmeans(dailykosMatrix, centers = k)
clusters = KMC$cluster

table(clusters, dailykosClusters)
table(clusters)

KmeansCluster1 = subset(dailykos, clusters == 1)
KmeansCluster2 = subset(dailykos, clusters == 2)
KmeansCluster3 = subset(dailykos, clusters == 3)
KmeansCluster4 = subset(dailykos, clusters == 4)
KmeansCluster5 = subset(dailykos, clusters == 5)
KmeansCluster6 = subset(dailykos, clusters == 6)
KmeansCluster7 = subset(dailykos, clusters == 7)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))
