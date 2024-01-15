# Data maintenance packages 
library(tidyverse)

# Load a png
library(png)
testImg <- readPNG("~/Downloads/KPMP_uS-X002Y010.png")

# Blur image
library(spatstat)

# Format data-------------------------------------------------------------------

# Convert pixels to a data.frame
convert_df <- function(the_mat, the_name) {
  the_mat %>% 
    data.frame() %>% 
    `colnames<-`(paste0("X", 1:ncol(the_mat))) %>%
    pivot_longer(1:ncol(the_mat)) %>%
    rename(X = name) %>%
    mutate(Y = rep(paste0("Y", nrow(the_mat):1), each = ncol(the_mat))) %>%
    relocate(X) %>%
    rename(!!the_name := value)
}

# Take red, green, and blue channels, and calculate the average pixel value
Img_DF <- Reduce(left_join, list(convert_df(blur(im(testImg[,,1]), sigma = 8)$v, "Red"),
                                 convert_df(blur(im(testImg[,,2]), sigma = 8)$v, "Green"),
                                 convert_df(blur(im(testImg[,,3]), sigma = 8)$v, "Blue"))) 
Img_DF$X <- gsub("X", "", Img_DF$X) %>% as.numeric()
Img_DF$Y <- gsub("Y", "", Img_DF$Y) %>% as.numeric()

# Let's briefly look at distributions
library(patchwork)
(ggplot(Img_DF, aes(x = Red)) + geom_histogram(color = "black") + theme_bw() + ggtitle("Red")) +
(ggplot(Img_DF, aes(x = Green)) + geom_histogram(color = "black") + theme_bw() + ggtitle("Green")) +
(ggplot(Img_DF, aes(x = Blue)) + geom_histogram(color = "black") + theme_bw() + ggtitle("Blue"))

# K-Means-----------------------------------------------------------------------

KMeans <- Img_DF %>% mutate(Cluster = as.factor(kmeans(Img_DF[,c("Red", "Green", "Blue")], centers = 3)$cluster))
ggplot(KMeans, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()

# KCC---------------------------------------------------------------------------

library(flexclust) # install.packages("flexclust")

KCC <- kcca(Img_DF[,c("Red", "Green", "Blue")], k = 3)
KCentroid <- Img_DF %>% mutate(Cluster = as.factor(KCC@cluster))
ggplot(KCentroid, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()

# t-SNE + KNN ------------------------------------------------------------------

library(M3C) # BiocManager::install("M3C")
library(e1071)

# Do t-sne on the transpose of the data.frame
uniqColors <- unique(Img_DF[,c("Red", "Green", "Blue")])
TSN <- tsne(t(uniqColors))
TSN_UC <- uniqColors %>% mutate(Cluster = kmeans(TSN$data, 3)$cluster)

# Merge data
TSN_KM <- left_join(Img_DF, TSN_UC, by = c("Red", "Green", "Blue")) %>% mutate(Cluster = as.factor(Cluster))
ggplot(TSN_KM, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()

# PCA + KNN---------------------------------------------------------------------
Kcc
# Generate the principal component analysis
pca <- princomp(Img_DF[,c("Red", "Green", "Blue")])
PCA_KM <- Img_DF %>% mutate(Cluster = as.factor(kmeans(pca$scores, 3)$cluster))
ggplot(PCA_KM, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()





