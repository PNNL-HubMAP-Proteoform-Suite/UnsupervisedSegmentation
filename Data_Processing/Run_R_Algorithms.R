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
### Dimension Reduction Study---------------------------------------------------
################################################################################

# Subset down to blur images
targets <- Image_Metadata %>% filter(Blur == "X") %>% select(Path) %>% unique() %>% unlist() 
DRMeta <- Image_Metadata %>% filter(Path %in% targets)
DRPaths <- DRMeta %>%
  select(Path, ManualClusterNumber) %>%
  group_by(Path) %>%
  summarize(ClusterNum = n()) %>%
  ungroup() %>%
  mutate(
    Path = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Path, ".png"),
    Path = gsub("_Annotations", "", Path)
  ) 

## PCA + KCC ##
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/Dimension_Reduction/pca_kcc.R")

lapply(1:10, function(x) {
  apply_pca_kcc(
    in_path = DRPaths$Path[x],
    k = DRPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PCA_KCC_TXT/"
  )
})

## tSNE + KCC ## 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/Dimension_Reduction/tsne_kcc.R")

lapply(1:10, function(x) {
  apply_tsne_kcc(
    in_path = DRPaths$Path[x],
    k = DRPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/tSNE_KCC_TXT/"
  )
})

## SVD + KCC ## 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/Dimension_Reduction/svd_kcc.R")

lapply(1:10, function(x) {
  apply_svd_kcc(
    in_path = DRPaths$Path[x],
    k = DRPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/SVD_KCC_TXT/"
  )
})


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

###########
## CLARA ##
###########

# Run unmodified and blurred images 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/clara.R")

lapply(1:nrow(BlurPaths), function(x) {
  apply_clara(
    in_path = BlurPaths$Path[x],
    k = BlurPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_Blur_TXT/", # update when blurred
    blur = TRUE # Change to false when unblurred
  )
})

################
## SUPERCELLS ##
################

# Run unmodified and blurred images 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/supercells.R")

lapply(1:nrow(BlurPaths), function(x) {
  apply_supercells(
    in_path = BlurPaths$Path[x],
    k = BlurPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_TXT/",
    blur = TRUE
  )
})

################
## RECOLORIZE ##
################

# Run unmodified and blurred images 
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/recolorize.R")

lapply(1:nrow(BlurPaths), function(x) {
  apply_recolorize(
    in_path = BlurPaths$Path[x],
    k = BlurPaths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_TXT/",
    blur = TRUE
  )
})


################################################################################
### Full study------------------------------------------------------------------
################################################################################

# No need to re-run blur images! 
targets <- Image_Metadata %>% filter(Blur != "X") %>% select(Path) %>% unique() %>% unlist() 
Meta <- Image_Metadata %>% filter(Path %in% targets)
Paths <- Meta %>%
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

lapply(1:nrow(Paths), function(x) {
  apply_kmeans(
    in_path = Paths$Path[x],
    k = Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT/",
    blur = FALSE
  )
})

#########
## KCC ##
#########

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc.R")

cl <- makeCluster(6)
registerDoParallel(cl)
foreach(x = 1:nrow(Paths)) %dopar% {
  apply_kcc(
    in_path = Paths$Path[x],
    k = Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/",
    blur = TRUE
  )
}
stopCluster(cl)

###########
## CLARA ##
###########

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/clara.R")

cl <- makeCluster(6)
registerDoParallel(cl)
foreach(x = 1:nrow(Paths)) %dopar% {
  apply_clara(
    in_path = Paths$Path[x],
    k = Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_TXT/",
    blur = FALSE
  )
}
stopCluster(cl)

################
## SUPERCELLS ##
################

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/supercells.R")

cl <- makeCluster(6)
registerDoParallel(cl)
foreach(x = 1:nrow(Paths)) %dopar% {
  apply_supercells(
    in_path = Paths$Path[x],
    k = Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_TXT/",
    blur = FALSE
  )
}
stopCluster(cl)

################
## RECOLORIZE ##
################

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/recolorize.R")

cl <- makeCluster(6)
registerDoParallel(cl)
foreach(x = 1:nrow(Paths)) %dopar% {
  apply_recolorize(
    in_path = Paths$Path[x],
    k = Paths$ClusterNum[x],
    out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_TXT/",
    blur = FALSE
  )
}
stopCluster(cl)

################################################################################
### Full tissue-----------------------------------------------------------------
################################################################################

source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kmeans.R")

apply_kmeans(in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", k = 4, blur = FALSE,
             out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/")

source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc.R")

apply_kcc(in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", k = 4, blur = TRUE,
             out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/")

source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/clara.R")

apply_clara(in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", k = 4, blur = FALSE,
          out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/")

source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/recolorize.R")

apply_recolorize(in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", k = 4, blur = FALSE,
            out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/")

source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/supercells.R")

apply_supercells(in_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", k = 4, blur = FALSE,
                 out_path = "~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/")







