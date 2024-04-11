library(tidyverse)
library(doParallel)
library(foreach)

# Start an image data.frame
images <- data.frame(
  Original = rep(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/NewCohort/Scaled_Down/", full.names = T), 3),
  K = rep(3:5, each = 9)
) 

#############
## K-MEANS ##
#############

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kmeans.R")

# Add outputs 
images <- images %>%
  mutate(
    KM_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KMeans", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KMeans_", y, ".png"))
    }), 
    KM_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Scaled_Down", replacement = "KMeans", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KMeans_", y, ".txt"))
    })
  ) 

cl <- makeCluster(8)
registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% {
  apply_kmeans(
    in_path = images$Original[x],
    out_path_data = images$KM_Out_Data[x],
    out_path_image = images$KM_Out[x],
    k = images$K[x]
  )
}
stopCluster(cl)

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


