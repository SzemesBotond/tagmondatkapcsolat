#1. Cluster Analysis
library(cluster)
#adatokat dataframe-be, cluster belso aranyok, vagy relatív frekvencia alapjan
#ez a mondatkapcs_relfreq.R, vagy a mondatkapcs_arany.R fájlból is megtehető, vagy a kiírt Excel táblázatból bemásolható (itt az utóbbit választottam)
data_cluster <- read.table(file = "clipboard", 
                           sep = "\t", header=TRUE)
rownames(data_cluster) <- unlist(data_cluster$Filename)

#cluster-ek létrehozása
d <- dist(data_cluster[,5:23], method = "manhattan") 
fit <- hclust(d, method="ward.D")
dend <- as.dendrogram(fit)
#ertekek szinezese
library(dendextend)
library(colorspace)
color_unique_labels <- function(dend, ...) {
  n_unique_labels <- length(unique(sub('(.*)(_.*)','\\1', labels(dend), perl=T)))
  colors <- colorspace::diverging_hcl(n_unique_labels, "Berlin")
  labels_number <- as.numeric(factor(sub('(.*)(_.*)','\\1', labels(dend), perl=T)))
  labels_colors(dend) <- colors[labels_number]
  dend
}
dend2 <- color_unique_labels(dend)
labels(dend2) <- sub('(.*_)(.*)','\\2', labels(dend), perl=T)
par(cex=0.7, mar=c(5, 8, 4, 1))
plot(dend2) 
title(main = "5.3. Manhattan-távolság\nKapcsolattípusok egymáshoz viszonyított aránya és relatív gyakorisága alapján", adj=0.5, cex.lab = 0.7)
title(sub = "33 regény", adj=1.0, line=-15)

#ellenőrzés 33 regényre (12 szerző)
library(mclust )
adjustedRandIndex(cutree(fit, 12), data_cluster[,2])
library(fossil)
rand.index(cutree(fit, 12), as.numeric(as.factor(data_cluster$Szerző)))

#2. PCA
pca <- prcomp(data_cluster[,6:16], scale=T)
library(ggfortify)
library(ggplot2)
data_cluster$Szerző <- as.factor(data_cluster$Szerző)
autoplot(pca, data = data_cluster, colour = "Szerző", shape=F, label.size = 3, loadings = TRUE,  loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("5.4. Főkomponens-analízis\nKapcsolattípusok relatív gyakorisága szerint", subtitle = "33 Regény")+
  theme(plot.title = element_text(size=12)) +
  theme(plot.subtitle = element_text(hjust=1.0))+
  theme(legend.position = "none")



