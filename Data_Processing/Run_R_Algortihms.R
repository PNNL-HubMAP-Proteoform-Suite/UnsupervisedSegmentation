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

# Add outputs 
images <- images %>%
  mutate(
    KC_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCC_", y, ".png"))
    }), 
    KC_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KCC", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCC_", y, ".txt"))
    })
  ) 

cl <- makeCluster(8)
registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% {
  apply_kcc(
    in_path = images$Original[x],
    out_path_data = images$KC_Out_Data[x],
    out_path_image = images$KC_Out[x],
    k = images$K[x]
  )
}
stopCluster(cl)

##################
## KCC + Blur10 ##
##################

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc_blur10.R")

# Add outputs 
images <- images %>%
  mutate(
    KCC_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KCC_Blur10", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCCBlur10_", y, ".png"))
    }), 
    KCC_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KCC_Blur10", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCCBlur10_", y, ".txt"))
    })
  ) 

cl <- makeCluster(8)
registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% {
  kcc_blur10(
    in_path = images$Original[x],
    out_path_data = images$KCC_Out_Data[x],
    out_path_image = images$KCC_Out[x],
    k = images$K[x]
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

