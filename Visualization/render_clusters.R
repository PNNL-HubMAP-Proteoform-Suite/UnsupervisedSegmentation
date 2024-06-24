library(data.table)
library(tidyverse)

render_cluster <- function(data, colors, order) {
  
  colorList <- colors
  names(colorList) <- order
  
  data %>%
    mutate(Height = 1:nrow(.)) %>%
    pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
    rename(Cluster = value, Width = name) %>%
    mutate(Width = gsub("V", "", Width) %>% as.numeric(),
           Cluster = as.factor(Cluster)) %>% 
    ggplot(aes(x = Width, y = Height, fill = Cluster)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_manual(values = colorList) +
    theme_void() +
    theme(legend.position = "none")
  
}

## Clusters need to be matched manually. Matches are tracked in the Annotation Summary csv

# KMeans Blur-------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_TXT", 
                        gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, Kmeans.Blur]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_PNG", 
                 gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# KMeans------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
tile <- 30
root <- unique(Image_Metadata$Path)[tile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                        gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KMeans]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG", 
                 gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# KCC---------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT", 
                        gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KCC]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG", 
                 gsub("_Annotations", "_KCC.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# KCC Blur----------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/", 
                        gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KCC.Blur]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", 
                 gsub("_Annotations", "_KCC.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# Clara-------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 1
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_TXT/", 
                        gsub(pattern = "Annotations", replacement = "Clara.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, Clara]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG", 
                 gsub("_Annotations", "_Clara.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# PyTorch-----------------------------------------------------------------------

Image_Metadata <- fread("/Users/lewi052/Imaging_3D/Segmentation/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
subtile <- 30
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_mask", gsub("_Annotations", ".csv", root)))
colnames(data) <- c("X", "Y", "Cluster")
Smaller <- data %>%
  mutate(X = factor(X, levels = 1:max(X))) %>%
  pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
  arrange(Y) %>%
  select(-Y)
colnames(Smaller) <- paste0("V", colnames(Smaller))
Smaller <- rev(Smaller)
Smaller <- apply(Smaller, 2, rev)
Smaller <- as.data.frame(Smaller)
fwrite(Smaller, file.path(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_fixed_clusters/", 
                                    gsub("_Annotations", ".txt", root))), quote = F, row.names = F, sep = "\t")

plot <- render_cluster(Smaller,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, PyTorch]))
plot

ggsave(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_fixed_clusters/", 
                 gsub("_Annotations", ".png", root)),  plot = plot,
       units = "px", height = nrow(Smaller), width = ncol(Smaller))

# PyTorch Blur------------------------------------------------------------------

Image_Metadata <- fread("/Users/lewi052/Imaging_3D/Segmentation/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 1
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_mask_blurred", paste0("blurred_", gsub("_Annotations", ".csv", root))))
colnames(data) <- c("X", "Y", "Cluster")
Smaller <- data %>%
  mutate(X = factor(X, levels = 1:max(X))) %>%
  pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
  arrange(Y) %>%
  select(-Y)
colnames(Smaller) <- paste0("V", colnames(Smaller))
Smaller <- rev(Smaller)
Smaller <- apply(Smaller, 2, rev)
Smaller <- as.data.frame(Smaller)
fwrite(Smaller, file.path(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_Blurred_fixed_clusters_masks/", 
                                    gsub("_Annotations", ".txt", root))), quote = F, row.names = F, sep = "\t")

plot <- render_cluster(Smaller,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, PyTorch.Blur]))
plot

ggsave(file.path("/Users/lewi052/Imaging_3D/Segmentation/Pytorch_Blurred_fixed_clusters/", 
                 gsub("_Annotations", ".png", root)),  plot = plot,
       units = "px", height = nrow(Smaller), width = ncol(Smaller))


# pyImSeg-----------------------------------------------------------------------

Image_Metadata <- fread("/Users/lewi052/Imaging_3D/Segmentation/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
subtile <- 6
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_masks", paste0("mask_", gsub("_Annotations", ".csv", root))))
colnames(data) <- c("X", "Y", "Cluster")
data$Cluster <- data$Cluster + 1
Smaller <- data %>%
  mutate(X = factor(X, levels = 1:max(X))) %>%
  pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
  arrange(Y) %>%
  select(-Y)
colnames(Smaller) <- paste0("V", colnames(Smaller))
Smaller <- rev(Smaller)
Smaller <- apply(Smaller, 2, rev)
Smaller <- as.data.frame(Smaller)
fwrite(Smaller, file.path(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_fixed_clusters_masks/", 
                                    gsub("_Annotations", ".txt", root))), quote = F, row.names = F, sep = "\t")

plot <- render_cluster(Smaller,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, pyImSeg]))
plot

ggsave(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_fixed_clusters/", 
                 gsub("_Annotations", ".png", root)),  plot = plot,
       units = "px", height = nrow(Smaller), width = ncol(Smaller))

# pyImSeg Blur------------------------------------------------------------------

Image_Metadata <- fread("/Users/lewi052/Imaging_3D/Segmentation/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_Blurred_masks", paste0("mask_blurred_", gsub("_Annotations", ".csv", root))))
colnames(data) <- c("X", "Y", "Cluster")
data$Cluster <- data$Cluster + 1
Smaller <- data %>%
  mutate(X = factor(X, levels = 1:max(X))) %>%
  pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
  arrange(Y) %>%
  select(-Y)
colnames(Smaller) <- paste0("V", colnames(Smaller))
Smaller <- rev(Smaller)
Smaller <- apply(Smaller, 2, rev)
Smaller <- as.data.frame(Smaller)
fwrite(Smaller, file.path(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_Blurred_fixed_clusters_masks/", 
                                    gsub("_Annotations", ".txt", root))), quote = F, row.names = F, sep = "\t")

plot <- render_cluster(Smaller,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, pyImSeg.Blur]))
plot

ggsave(file.path("/Users/lewi052/Imaging_3D/Segmentation/pyImSeg_Blurred_fixed_clusters/", 
                 gsub("_Annotations", ".png", root)),  plot = plot,
       units = "px", height = nrow(Smaller), width = ncol(Smaller))



