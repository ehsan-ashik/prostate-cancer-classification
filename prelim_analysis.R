install.packages('Rtsne')
install.packages('ggfortify')
install.packages("wesanderson")
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("mclust")
install.packages("fpc")

# Load
library(wesanderson)
library(Rtsne)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(mclust)  
library(fpc)

# input data and summary
dataset <- read.csv(file = '', header = TRUE)

head(dataset[2, -1])

names(dataset)[1] = 'ID_REF'

View(dataset)

summary(dataset[dataset$V1==1, 408])
names(dataset)

scaled.dataset <- scale(dataset[,-1])

View(scaled.dataset)



# PCA

pca_result <- prcomp(scaled.dataset)

summary(pca_result)

plot(pca_result$x[,1:2], pch = as.character(dataset$ID_REF),
     col = dataset$ID_REF, main = "PCA output")  


autoplot(pca_result, data = dataset, colour = 'label', label.size = 9)



# tSNE

Labels<-dataset$ID_REF
dataset$ID_REF<-as.factor(dataset$ID_REF)

## for plotting
colors = rainbow(length(unique(dataset$ID_REF)))
names(colors) = unique(dataset$ID_REF)


tsne <- Rtsne(scaled.dataset, dims = 2, perplexity=40, verbose=TRUE, max_iter = 1000)
nm <- 1:2565
head(nm)

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels = nm, col=colors[dataset$ID_REF])



#pca top x

pca <- prcomp(scaled.dataset, center = TRUE)

names(pca)

print(pca$sdev)

plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))

pc.use <- 20 # explains 80% of variance
trunc <- pca$x[,1:pc.use] %*% t(pca$rotation[,1:pc.use])

summary(trunc)

#and add the center (and re-scale) back to data
if(pca$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/pca$scale)
}
if(pca$center != FALSE){
  trunc <- scale(trunc, center = -1 * pca$center, scale=FALSE)
}
dim(trunc)




# indices of top 500 genes on PC1 by absolute value
values1 <- order(abs(pca$rotation[,1]), decreasing=TRUE)[1:100]

# indices of top 500 genes on PC2 by absolute value
values2 <- order(abs(pca$rotation[,2]), decreasing=TRUE)[1:100]

View(order(pca_result$rotation[,1], decreasing=TRUE))

plot(pca_result$x[,1:2], pch = as.character(dataset$ID_REF),
     col = dataset$ID_REF, main = "PCA output") 

summary(pca_result)
pca_result

dataset_t = setNames(data.frame(t(dataset[,-1])), dataset[,1])
scaled.dataset <- scale(dataset_t[,-1])
View(scaled.dataset)

pca <- prcomp(scaled.dataset, center = TRUE)

summary(pca)

topN <- 50
load.rot <- pca$rotation
names(load.rot[,1][order(abs(load.rot[,1]),decreasing=TRUE)][1:topN])

Loadings <- as.data.frame(pca$rotation[,1:3])
Loadings

t <- Loadings[order(abs(Loadings$PC1), decreasing = TRUE)[1:50],]
t

plot(t[,1:2], pch = as.character(dataset$ID_REF),
     col = dataset$ID_REF, main = "PCA output") 



# pca new
# PCA GROUP
dataset_t = setNames(data.frame(t(dataset[,-1])), dataset[,1])

scaled.dataset <- scale(dataset_t)

View(dataset_t)
View(scaled.dataset)

dataset_t = scaled.dataset

type <- rownames(dataset_t)

# prostate vs control
type[1:809] = 'PC'
type[810:850] = 'CTL'

# negative vs control
type[1:241] = 'NPB'
type[242:282] = 'CTL'

type <- factor(type, levels = c("NPB", "CTL"))

# prostate vs control vs negative
type[1:809] = 'PC'
type[810:850] = 'CTL'
type[851:1091] = 'NPB'

type <- factor(type, levels = c("PC", "CTL", "NPB"))

type


Labels<-dataset$ID_REF

## for plotting
colors = rainbow(length(unique(type)))

pca <- prcomp(dataset_t, center = TRUE)

dataset_t$type = type
dataset_t$type<-as.factor(dataset_t$type)
names(colors) = unique(dataset_t$type)

plot(pca$x, t='n', main="PCA")
text(pca$x, labels = dataset_t$type , col=wes_palette(n=3, name="BottleRocket2")[dataset_t$type])


# tSNE
dataset_t$type = type
Labels<-dataset$ID_REF
dataset_t$type<-as.factor(dataset_t$type)

## for plotting
colors = rainbow(length(unique(type)))
names(colors) = unique(dataset_t$type)


## Executing the algorithm on curated data 2
tsne <- Rtsne(dataset_t, dims = 2, perplexity=40, verbose=TRUE, max_iter = 2000)

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels = dataset_t$type , col=wes_palette(n=3, name="BottleRocket2")[dataset_t$type])




# clustering

# input 
df <- read.csv(file = '', header = TRUE)

#remove missing
df1 <- na.omit(df)

df1 <- data.frame(df)
df = setNames(data.frame(t(df1[,-1])), df1[,1])

# silhoutte

avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25) #Mclust(df, G = k) 
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}


#k means k = 2,3,4

# k = 2
k2 <- kmeans(df, centers = 2, nstart = 25)

# plot k = 2
fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")

# k = 3
k3 <- kmeans(df, centers = 3, nstart = 25)

# plot k = 3
fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")

# k = 4
k4 <- kmeans(df, centers = 4, nstart = 25)

# plot k = 4
fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")

# k-means silhoutte
k.values <- 2:4

# extract avg silhouette for 2-4 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# avg silhoutte values
avg_sil_values


#EM Clustering

fit2 <- Mclust(df, G = 2)

plot(fit2, what = "classification", title("k = 2"))

