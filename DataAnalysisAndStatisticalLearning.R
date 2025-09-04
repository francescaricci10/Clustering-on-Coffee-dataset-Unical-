# "Data Analysis and Statistical Models - Project Work"
# "Master in Artificial Intelligence and Data Science a.a. 2024/2025"
##### author: "Marco Longo - Francesca Ricci - Maria Rotella"

library(pgmm)
data(coffee)
head(coffee)

# utils
colors2 <- c("gold", "darkgreen")
colors3 <- c("darkgreen", "gold", "firebrick")
colors6 <- c("darkgreen", "gold", "orange3", "firebrick", "dodgerblue", "darkslategray")


# mostriamo le varietà
library(ggplot2)
df <- coffee
df$Variety <- factor(coffee$Variety, levels = c(1, 2), labels = c("arabica", "robusta"))
ggplot(df, aes(x = Caffine, y = `Free Acid`, color = Variety, size = Fat)) +
  geom_point() +
  scale_color_manual(values = colors2) + 
  labs(title = "Relation between Caffine, Free Acid and Fat", 
       x = "Caffine", 
       y = "Free Acid") +
  theme_minimal()


# preprocessing: costruiamo un id custom
df$Country <- as.character(df$Country)
df$Country[df$Country == "Costadav"] <- "cdavorio"
df$Country_Count <- ave(df$Country, df$Country, FUN = seq_along)
row.names(df) <- with(df, 
                      paste0(
                        tolower(substr(Country, 1, 3)),    # prefisso paese
                        Country_Count,                     # progressivo
                        ifelse(Variety == "arabica", "_a", "_r")   # Arabica o Robusta
                      ))
df$Country_Count <- NULL

# togliamo country e variety
df.to.scale <- df[, -c(1, 2)] 
head(df.to.scale)

# scale
df.num <- scale(df.to.scale)

# hopkins
library(clustertend)
set.seed(11)
random.df <- apply(df.num, 2, function(x){runif(length(x), min(x), max(x))})
random.df <- as.data.frame(random.df)
hopkins(random.df, nrow(random.df)-1)
hopkins(df.num, nrow(df.num)-1)


## Analisi dei componenti principali (PCA)

library(FactoMineR)

pca.res <- PCA(df.to.scale, ncp = 5, scale.unit = TRUE, graph = FALSE)
df.pca <- as.data.frame(pca.res$ind$coord)

library(factoextra)
eig.val <- get_eigenvalue(pca.res)
eig.val

fviz_eig(pca.res, 
         addlabels = TRUE, ylim = c(0, 50), 
         barfill = "gold", barcolor = "darkgreen", linecolor = "darkgreen",         
         linetype = "dashed", ggtheme = theme_minimal() 
)

var <- get_pca_var(pca.res)
var$contrib

fviz_pca_var(pca.res, col.var = "contrib", repel = FALSE, 
             col.circle = "darkgreen", labelsize = 4, gradient.cols = colors3)


library(corrplot)
colors.palette <- colorRampPalette(colors3)(200)
corrplot(var$contrib, 
         is.corr = FALSE, 
         col = colors.palette, 
         tl.col = "darkgreen",
         cl.ratio = 0.2,
         cl.align = "l")

# Contributions of variables to PC1
fviz_contrib(pca.res, choice = "var", axes = 1, top = 10, fill= "darkgreen")
# Contributions of variables to PC2
fviz_contrib(pca.res, choice = "var", axes = 2, top = 10, fill= "darkgreen")
# Contributions of variables to PC3
fviz_contrib(pca.res, choice = "var", axes = 3, top = 10, fill= "darkgreen")


## Misure di distanza per il clustering

dist.eucl <- dist(df.num, method = "euclidean")
round(as.matrix(dist.eucl)[1:9, 1:9], 1)

dist.manh <- dist(df.num, method = "manhattan")
round(as.matrix(dist.manh)[1:9, 1:9], 1)

fviz_dist(dist.eucl, gradient = list(low=colors3[1], mid=colors3[2], high=colors3[3])) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 7, lineheight = 2.5)) +  
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Distanza Euclidea")

fviz_dist(dist.manh, gradient = list(low=colors3[1], mid=colors3[2], high=colors3[3])) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 7, lineheight = 2.5)) +  
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = "Distanza di Manhattan")


## Clustering partizionale

fviz_nbclust(df.pca, kmeans, method = "wss", linecolor = "darkgreen") +
  ggplot2::theme_minimal(base_size = 14) +      
  ggplot2::ggtitle("Optimal Number of Clusters") + 
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title.x = ggplot2::element_text(size = 11),          
    axis.title.y = ggplot2::element_text(size = 11),          
    axis.text = ggplot2::element_text(size = 11),                              
    panel.grid = ggplot2::element_line(color = "lightgray", size = 0.5),       
    legend.position = "none"                                                  
  ) +
  ggplot2::xlab("Number of Clusters (K)") +        
  ggplot2::ylab("Total Within Sum of Squares (WSS)")     

set.seed(123)
fviz_nbclust(df.num, kmeans, nstart=50, method="gap_stat", nboot=50, linecolor="darkgreen") +
  ggplot2::theme_minimal(base_size = 14) +      
  ggplot2::ggtitle("Optimal Number of Clusters") + 
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title.x = ggplot2::element_text(size = 11),          
    axis.title.y = ggplot2::element_text(size = 11),          
    axis.text = ggplot2::element_text(size = 11),                              
    panel.grid = ggplot2::element_line(color = "lightgray", size = 0.5),       
    legend.position = "none"                                                  
  ) +
  ggplot2::xlab("Number of Clusters (K)") +        
  ggplot2::ylab("Gap Statistic")     

set.seed(123)
fviz_nbclust(df.pca, kmeans, method = "silhouette", linecolor = "darkgreen") + 
    ggplot2::theme_minimal(base_size = 14) +      
  ggplot2::ggtitle("Optimal Number of Clusters") + 
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title.x = ggplot2::element_text(size = 11),          
    axis.title.y = ggplot2::element_text(size = 11),          
    axis.text = ggplot2::element_text(size = 11),                              
    panel.grid = ggplot2::element_line(color = "lightgray", size = 0.5),       
    legend.position = "none"                                                  
  ) +
  ggplot2::xlab("Number of Clusters (K)") +        
  ggplot2::ylab("Silhouette Width")

### K-Means

set.seed(123)
km3.res <- kmeans(df.pca, 3, nstart = 50)
km3.res$cluster
km3.res$centers
km3.res$size

cluster_means <- aggregate(df, by=list(cluster=km3.res$cluster), mean)
df.c.kmeans <- cbind(df, cluster = km3.res$cluster)
head(df.c.kmeans[, c(1, 2, 4, 8, 9, 10, ncol(df.c.kmeans))], 14)

cl <- km3.res$cluster
pairs(df.num[, 1:8], gap=0, pch=cl, col=colors6[cl])

fviz_cluster(km3.res, df.num, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, 
             palette = colors6, ggtheme = theme_minimal()
)


library(cluster)
sil3 <- silhouette(km3.res$cluster, dist(df.pca))
p.sil3 <- fviz_silhouette(sil3, palette = colors6)
p.sil3

set.seed(123)
km2.res <- kmeans(df.pca, 2, nstart = 50)
km2.res$centers
km2.res$size

fviz_cluster(km2.res, df.num, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, 
             palette = colors6, ggtheme = theme_minimal()
)

sil2 <- silhouette(km2.res$cluster, dist(df.pca))
p.sil2 <- fviz_silhouette(sil2, palette=colors6)
p.sil2

set.seed(123)
km4.res <- kmeans(df.pca, 4, nstart = 50)
km4.res$centers
km4.res$size

fviz_cluster(km4.res, df.num, ellipse.type = "euclid",star.plot = TRUE,repel = TRUE, 
             palette = colors6, ggtheme = theme_minimal()
)

sil4 <- silhouette(km4.res$cluster, dist(df.pca))
p.sil4 <- fviz_silhouette(sil4, palette=colors6)
p.sil4


### K-Medoids

set.seed(123)
pam2.res <- pam(df.num, 2)

df.c.kmedoid <- cbind(df, cluster = pam2.res$cluster)
head(df.c.kmedoid[, c(1, 2, 4, 8, 9, 10, ncol(df.c.kmedoid))], 10)

pam2.res$medoids

cl <- pam2.res$cluster
pairs(df.num[, 1:10], gap=0, pch=cl, col=colors6[cl])

fviz_cluster(pam2.res, palette = colors2, ellipse.type = "t", repel = TRUE, 
             ggtheme = theme_classic()
)

sil.2 <- silhouette(pam2.res$cluster, dist(df.num))
p.sil.2 <- fviz_silhouette(sil.2, palette=colors6)
p.sil.2

set.seed(123)
pam3.res <- pam(df.num, 3)

df.c.kmedoid <- cbind(df, cluster = pam3.res$cluster)
head(df.c.kmedoid[, c(1, 2, 4, 8, 9, 10, ncol(df.c.kmedoid))], 10)

pam3.res$medoids
cl <- pam3.res$cluster
pairs(df.num[, 1:10], gap=0, pch=cl, col=colors6[cl])

fviz_cluster(pam3.res, palette = colors6, ellipse.type = "t", repel = TRUE, 
             ggtheme = theme_classic()
)


sil.3 <- silhouette(pam3.res$cluster, dist(df.num))
p.sil.3 <- fviz_silhouette(sil.3, palette=colors6)
p.sil.3


## Clustering gerarchico

set.seed(123)
res.dist.eu <- dist(df.pca, method = "euclidean")
round(as.matrix(res.dist.eu)[1:9, 1:9],digits = 2)

set.seed(123)
res.dist.ma <- dist(df.pca, method = "manhattan")
round(as.matrix(res.dist.ma)[1:9, 1:9],digits = 2)

set.seed(123)
res.hc.eu.1 <- hclust(d = res.dist.eu, method = "ward.D2")
fviz_dend(res.hc.eu.1, 
            cex = 0.65, k = 3, k_colors = colors6, rect = TRUE, rect_border = "grey",       
            rect_fill = TRUE, horiz = FALSE, lwd = 0.8) +                 
    theme_minimal() +                    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
    labs(title = "Cluster Dendogram", subtitle = "Ward.2 linkage and Euclidean distance")

res.hc.ma.1 <- hclust(d = res.dist.ma, method = "ward.D2")
fviz_dend(res.hc.ma.1, 
            cex = 0.65, k = 3, k_colors = colors6, rect = TRUE, rect_border = "grey",       
            rect_fill = TRUE, horiz = FALSE, lwd = 0.8) +                 
    theme_minimal() +                    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
    labs(title = "Cluster Dendogram", subtitle = "Ward.2 linkage and Manhattan distance")

  

# Ward.2 evaluation with Euclidean
res.coph.eu <- cophenetic(res.hc.eu.1)
cor(res.dist.eu, res.coph.eu)
# Ward.2 evaluation with Manhattan
res.coph.ma <- cophenetic(res.hc.ma.1)
cor(res.dist.ma, res.coph.ma)

grp23 <- cutree(res.hc.eu.1, k = c(2,3))
table(grp2 = grp23[,"2"], grp3 = grp23[,"3"])

grp1 <- cutree(res.hc.eu.1, k = 3)
fviz_cluster(list(data = df.num, cluster = grp1),
             palette = colors3, ellipse.type = "convex", repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal())

res.hc2 <- hclust(res.dist.eu, method = "complete")
cor(res.dist.eu, cophenetic(res.hc2))

res.hc3 <- hclust(res.dist.eu, method = "average")
cor(res.dist.eu, cophenetic(res.hc3))

res.hc4 <- hclust(res.dist.eu, method = "centroid")
cor(res.dist.eu, cophenetic(res.hc4))

fviz_dend(res.hc3, cex = 0.65, k = 3,                     
          k_colors = colors6, rect = TRUE, rect_border = "grey", rect_fill = TRUE,            
          horiz = FALSE, lwd = 0.8) +                 
  theme_minimal() +                    
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10))

grp3 <- cutree(res.hc3, k = 3)
fviz_cluster(list(data = df.num, cluster = grp3),
             palette = colors3, ellipse.type = "convex", repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal())

fviz_dend(res.hc3, cex = 0.8, k = 3, k_colors = colors6, type = "circular")

fviz_dend(res.hc3, k = 3, k_colors = colors6, type = "phylogenic", repel = TRUE)


grp2 <- cutree(res.hc3, k = 4)
table(grp2)

fviz_cluster(list(data = df.num, cluster = grp2),
             palette = colors6, ellipse.type = "convex", repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal()
)

### Le funzioni AGNES e DIANA

res.agnes <- agnes(x = df.pca, stand = TRUE, metric = "euclidean", method = "ward")
fviz_dend(res.agnes, 
          cex = 0.65, k = 3, k_colors = colors6, rect = TRUE, rect_border = "grey",       
          rect_fill = TRUE, horiz = FALSE, lwd = 0.8) +                 
  theme_minimal() +                    
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
  labs(title = "Cluster Dendogram - AGNES")


res.diana <- diana(x = df.pca, stand = TRUE, metric = "euclidean")
fviz_dend(res.diana, 
          cex = 0.65, k = 3, k_colors = colors6, rect = TRUE, rect_border = "grey",       
          rect_fill = TRUE, horiz = FALSE, lwd = 0.8) +                 
  theme_minimal() +                    
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
  labs(title = "Cluster Dendogram - DIANA")


library(dendextend)

dend1 <- as.dendrogram (res.agnes)
dend2 <- as.dendrogram (res.diana)
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2, 
    highlight_distinct_edges = TRUE,
    common_subtrees_color_lines = TRUE,
    common_subtrees_color_branches = TRUE,
    main = paste("entanglement =", round(entanglement(dend_list), 2))
)


## Metodi di validazione

library(fpc)

stats_kmeans2 <- cluster.stats(dist(df.num), km2.res$cluster)
stats_kmeans3 <- cluster.stats(dist(df.num), km3.res$cluster)
stats_kmeans4 <- cluster.stats(dist(df.num), km4.res$cluster)
stats_kmed2 <- cluster.stats(dist(df.num), pam2.res$clustering)
stats_kmed3 <- cluster.stats(dist(df.num), pam3.res$clustering)
stats_ger_euclid2 <- cluster.stats(dist(df.num), cutree(res.hc.eu.1, 2))
stats_ger_euclid3 <- cluster.stats(dist(df.num), cutree(res.hc.eu.1, 3))
stats_ger_man2 <- cluster.stats(dist(df.num), cutree(res.hc.ma.1, 2))
stats_ger_man3 <- cluster.stats(dist(df.num), cutree(res.hc.ma.1, 3))
stats_ger_dian2 <- cluster.stats(dist(df.num), cutree(res.diana, 2))
stats_ger_dian3 <- cluster.stats(dist(df.num), cutree(res.diana, 3))
stats_ger_agnes2 <- cluster.stats(dist(df.num), cutree(res.agnes, 2))
stats_ger_agnes3 <- cluster.stats(dist(df.num), cutree(res.agnes, 3))

table <- data.frame(
  silhouette = c(
    stats_kmeans2$avg.silwidth, 
    stats_kmeans3$avg.silwidth, 
    stats_kmeans4$avg.silwidth, 
    stats_kmed2$avg.silwidth, 
    stats_kmed3$avg.silwidth, 
    stats_ger_euclid2$avg.silwidth, 
    stats_ger_euclid3$avg.silwidth, 
    stats_ger_man2$avg.silwidth, 
    stats_ger_man3$avg.silwidth, 
    stats_ger_dian2$avg.silwidth, 
    stats_ger_dian3$avg.silwidth, 
    stats_ger_agnes2$avg.silwidth,
    stats_ger_agnes3$avg.silwidth),
  
  dunn_index = c(
    stats_kmeans2$dunn, 
    stats_kmeans3$dunn, 
    stats_kmeans4$dunn, 
    stats_kmed2$dunn, 
    stats_kmed3$dunn, 
    stats_ger_euclid2$dunn, 
    stats_ger_euclid3$dunn, 
    stats_ger_man2$dunn, 
    stats_ger_man3$dunn, 
    stats_ger_dian2$dunn, 
    stats_ger_dian3$dunn,
    stats_ger_agnes2$dunn,
    stats_ger_agnes3$dunn)
)

rownames(table) <- c(
  "k-means (k=2)", 
  "k-means (k=3)", 
  "k-means (k=4)", 
  "k-medoids (K=2)", 
  "k-medoids (K=3)", 
  "Hierarchical - Euclidean Dist (K=2)", 
  "Hierarchical - Euclidean Dist (K=3)", 
  "Hierarchical - Manhattan Dist (K=2)", 
  "Hierarchical - Manhattan Dist (K=3)", 
  "Hierarchical - Diana (k=2)", 
  "Hierarchical - Diana (k=3)", 
  "Hierarchical - Agnes (K=2)",
  "Hierarchical - Agnes (K=3)")

print(table)

table(coffee$Variety, pam2.res$clustering)

stats_kmed2 <- cluster.stats(dist(df.num), coffee$Variety, pam2.res$clustering)
stats_kmed2$corrected.rand
stats_kmed2$vi


library(clValid)
stability <- clValid(df.num, nClust=2:5, 
                     clMethods = c("hierarchical", "kmeans", "pam"), 
                     validation = "stability")
summary(stability)


# Modelli Avanzati di Clustering

## Applicazione dei modelli GMM

library(mclust)
gmm_model <- Mclust(df.num)
summary(gmm_model)

table(gmm_model$classification)

df.scaled <- as.data.frame(df.num)
df.scaled$Cluster <- gmm_model$classification

head(gmm_model$z)

fviz_mclust(gmm_model, "classification", geom = "point", pointsize = 1.5, palette = colors6) +
  ggtitle("Cluster Analysis - GMM Classification") + labs(subtitle = "") + theme_minimal() 

fviz_mclust(gmm_model, "BIC", palette = "jco")

fviz_mclust(gmm_model, "uncertainty", palette = colors6) + theme_minimal() 

confmatrix <- function(A) {
  cf <- A
  G <- nrow(A)
  for (i in 1:(G - 1)) {
    max_row <- which.max(cf[i:G, i]) + (i - 1)
    if (max_row != i) {
      cf[c(i, max_row), ] <- cf[c(max_row, i), ]
      rownames(cf)[c(i, max_row)] <- rownames(cf)[c(max_row, i)]
    }
  }
  return(cf)
}

# Creazione della matrice di confusione tra GMM e K-means
optimal_classification <- km3.res$cluster
conf_matrix <- confmatrix(table(df.scaled$Cluster, optimal_classification))
conf_matrix


accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

ari <- adjustedRandIndex(df.scaled$Cluster, optimal_classification)
print(paste("Adjusted Rand Index (ARI):", round(ari, 2)))

## Parsimonious Gaussian Mixture Model (PGMM)

df.scaled <- df.scaled[, -ncol(df.scaled)]

pgmm_model <- pgmmEM(x = df.scaled, zstart = 1, loop=10)
summary(pgmm_model)

res <- table(coffee[,1], pgmm_model$map)
res

accuracy <- sum(diag(res)) / sum(res)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))


fviz_cluster(list(data = df.scaled, cluster = pgmm_model$map), palette=colors2)

pgmm_model2<-pgmmEM(x=df.scaled,zstart=2,modelSubset=c("UUU"))

table(coffee[,1],pgmm_model2$map)

fviz_cluster(list(data = df.scaled, cluster = pgmm_model2$map), palette=colors2)

## Misture di Modelli di Regressione Lineare

library(readxl)
library(flexmix)

df.scaled <- as.data.frame(df.scaled)

free_acid <- df.scaled$`Free Acid`
fat <- df.scaled$Fat
caffine <- df.scaled$Caffine
trigonelline <- df.scaled$Trigonelline
water <- df.scaled$Water


library(ggcorrplot)
cor_matrix <- cor(df.num, use = "complete.obs")

ggcorrplot(cor_matrix, 
           method = "circle", 
           lab = TRUE,        
           lab_size = 2.8,
           title = "Matrice di Correlazione delle Variabili",
           colors = colors3) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

palette <- c("gold", "darkgreen")
plot(free_acid, caffine, 
     col = palette[df$Variety], pch = 19, cex = 1.2,
     xlab = "Free Acid", ylab = "Caffine", 
     main = "Free Acid vs Caffine nelle Varietà di Caffè")
grid(col = "lightgray", lty = "dotted")
legend("bottomright", legend = unique(df$Variety), 
       col = palette[1:length(unique(coffee$Variety))], pch = 19, bty = "n")

### Finite Mixture Regression Model (FMR)

set.seed(1000)
X.fmr <- stepFlexmix(caffine ~ free_acid, k=2, nrep=10)
summary(X.fmr)

pred.X.fmr <- clusters(X.fmr)

plot(free_acid, caffine, col = colors6[pred.X.fmr], pch = 19, cex = 1.2,
     xlab = "Free Acid", ylab = "Caffine", main = "FMR")
grid(col = "lightgray", lty = "dotted")
legend("topleft", legend = c("Cluster1", "Cluster2"), col=colors6, pch=19, bty="n")

FMR1<-parameters(X.fmr)[1:2,1]
FMR2<-parameters(X.fmr)[1:2,2]

curve(FMR1[1]+FMR1[2]*x,col="darkgreen",add=TRUE)
curve(FMR2[1]+FMR2[2]*x,col="gold",add=TRUE)


cf.pred1.fmr<-confmatrix(table(coffee$Variety, clusters(X.fmr)))
cf.pred1.fmr

accuracy.fmr <-sum(diag(cf.pred1.fmr))/nrow(coffee)*100
accuracy.fmr

ARI.fmr<-adjustedRandIndex(coffee$Variety,clusters(X.fmr)) 
ARI.fmr

### Finite Mixture Regression Model with Covariates (FMRC)

set.seed(1000)

X.fmrc <- stepFlexmix(caffine ~ free_acid, k=2, nrep=10, 
                      concomitant = FLXPmultinom(~ free_acid+trigonelline+fat+water))
summary(X.fmrc)
pred.X.fmrc <- clusters(X.fmrc)

plot(free_acid, caffine,  col = colors6[pred.X.fmrc], pch = 19, cex = 1.2,
     xlab = "Free Acid", ylab = "Caffine", main = "FMRC")
grid(col = "lightgray", lty = "dotted")
legend("topleft", legend = c("Cluster1", "Cluster2"), col=colors6, pch=19, bty="n")

FMR1<-parameters(X.fmrc)[1:2,1]
FMR2<-parameters(X.fmrc)[1:2,2]

curve(FMR1[1]+FMR1[2]*x,col="darkgreen",add=TRUE)
curve(FMR2[1]+FMR2[2]*x,col="gold",add=TRUE)

cf.pred1.fmrc<-confmatrix(table(coffee$Variety, clusters(X.fmrc)))
cf.pred1.fmrc

accuracy.fmrc <-sum(diag(cf.pred1.fmrc))/nrow(coffee)*100
accuracy.fmrc

ARI.fmrc <-adjustedRandIndex(coffee$Variety,clusters(X.fmrc))
ARI.fmrc

error_MR <- (1-accuracy.fmr/100)*100
error_MRC <- (1-accuracy.fmrc/100)*100

table <- data.frame(
  MR = c(accuracy.fmr, error_MR, ARI.fmr),
  MRC = c(accuracy.fmrc, error_MRC, ARI.fmrc)
)

rownames(table) <- c("Accuracy (%)", "Misclassification Error (%)", "Adjusted Rand Index")
print(table)