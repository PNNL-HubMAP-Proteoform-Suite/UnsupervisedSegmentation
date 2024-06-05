library(tidyverse)
library(data.table)
library(doParallel)
library(foreach)

# Read Metadata
Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

# Create image path information
Image_Paths <- Image_Metadata %>%
  select(Path, ManualClusterNumber) %>%
  group_by(Path) %>%
  summarize(ClusterNum = n()) %>%
  ungroup() %>%
  mutate(
    Path = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Path, ".png"),
    Path = gsub("_Annotations", "", Path)
  )

################################################################################
### Blurring Study--------------------------------------------------------------
################################################################################

# Subset down to blur images
targets <- Image_Metadata %>% filter(Blur == "X") %>% select(Path) %>% unique() %>% unlist() 
BlurMeta <- Image_Metadata %>% filter(Path %in% targets)
BlurPaths <- BlurMeta %>%
  select(Path, ManualClusterNumber) %>%
  group_by(Path) %>%
  summarize(ClusterNum = n()) %>%
  ungroup() %>%
  mutate(
    Path = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Path, ".png"),
    Path = gsub("_Annotations", "", Path)
  ) 

#############
## K-Means ##
#############

# Run unmodified and blurred images 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kmeans.R")

lapply(1:nrow(BlurPaths), function(x) {
  apply_kmeans(
    in_path = BlurPaths$Path[x],
    k = BlurPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_TXT/", # update when blurred
    blur = TRUE # Change to false when unblurred
  )
})

#########
## KCC ##
#########

# Run unmodified and blurred images 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc.R")

lapply(1:nrow(BlurPaths), function(x) {
  apply_kcc(
    in_path = BlurPaths$Path[x],
    k = BlurPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/", # update when blurred
    blur = TRUE # Change to false when unblurred
  )
})

################################################################################
### Full study------------------------------------------------------------------
################################################################################

#############
## K-MEANS ##
#############

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kmeans.R")

lapply(1:nrow(Image_Paths), function(x) {
  apply_kmeans(
    in_path = Image_Paths$Path[x],
    k = Image_Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT/"
  )
})


#########
## KCC ##
#########

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc.R")

cl <- makeCluster(6)
registerDoParallel(cl)
foreach(x = 1:nrow(Image_Paths)) %dopar% {
  apply_kcc(
    in_path = Image_Paths$Path[x],
    k = Image_Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT/"
  )
}
stopCluster(cl)

#################
## T-SNE + KCC ##
#################

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/tsne_kcc.R")

# Add outputs 
images <- images %>%
  mutate(
    TSNE_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "TSNE_KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_TSNE-KCC_", y, ".png"))
    }), 
    TSNE_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "TSNE_KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_TSNE-KCC_", y, ".txt"))
    })
  ) 

cl <- makeCluster(8)
registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% {
  tsne_kcc(
    in_path = images$Original[x],
    out_path_data = images$TSNE_Out_Data[x],
    out_path_image = images$TSNE_Out[x],
    k = images$K[x]
  )
}
stopCluster(cl)

###############
## PCA + KCC ##
###############

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/pca_kcc.R")

# Add outputs 
images <- images %>%
  mutate(
    PCA_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "PCA_KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_PCA-KCC_", y, ".png"))
    }), 
    PCA_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "PCA_KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_PCA-KCC_", y, ".txt"))
    })
  ) 

cl <- makeCluster(8)
registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% {
  pca_kcc(
    in_path = images$Original[x],
    out_path_data = images$PCA_Out_Data[x],
    out_path_image = images$PCA_Out[x],
    k = images$K[x]
  )
}
stopCluster(cl)

