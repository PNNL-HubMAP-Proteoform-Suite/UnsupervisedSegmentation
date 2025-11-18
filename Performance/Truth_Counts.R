# Data maintenance packages 
library(tidyverse)
library(data.table)

## Load the correct metadata file
Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
#Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv")
#Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Root.csv")

#' @param truth A data.frame with cluster values per height (rows) and width (columns) 
#'     for the truth data 
#' @param predicted A data.frame with cluster values per height (rows) and width (columns)
#'     for the predicted values. Must be oriented the same way as truth. 
#' @param image The name of the image in the metadata file
#' @param model The name of the model column in the metadata file 
#' @param rm_row Remove first row and column on predicted
truth_counts <- function(truth, predicted, image, model, rm_row_col = FALSE) {
  
  # Pull the number of clusters
  NumClusters <- max(truth)
  
  # Make the submeta data.frame and the cluster conversion
  SubMeta <- Metadata %>% filter(Path == image)
  ClusterConvert <- SubMeta[[model]]
  names(ClusterConvert) <- SubMeta$ManualClusterNumber
  
  # Make function to pivot image data.frames
  make_pivot <- function(df, rm_step = FALSE) {
    
    if (rm_step) {
      
      df[2:nrow(df), 2:ncol(df)] %>%
        mutate(Height = 1:nrow(.)) %>%
        pivot_longer(cols = -Height) %>%
        rename(Width = name, Cluster = value) %>%
        mutate(Width = gsub("V", "", Width) %>% as.numeric(),
               Width = Width - 1,
               Width = as.integer(Width))
    
    } else {
      
      df %>%
        mutate(Height = 1:nrow(.)) %>%
        pivot_longer(cols = -Height) %>%
        rename(Width = name, Cluster = value) %>%
        mutate(Width = gsub("V", "", Width) %>% as.numeric(),
               Width = as.integer(Width))
    
    }
    

  }
  
  # Merge data.frames for counts 
  toCalc <- left_join(
    make_pivot(truth) %>% rename(TrueCluster = Cluster),
    make_pivot(predicted, rm_row_col) %>% mutate(Cluster = map_int(Cluster, function(x) {which(ClusterConvert == x)})) %>% rename(PredictedCluster = Cluster),
    by = c("Height", "Width")
  )
  
  # Conduct count per cluster
  stats <- do.call(rbind, lapply(1:NumClusters, function(x) {
    
    accuracy_mat <- toCalc %>%
      mutate(
        Result = ifelse(TrueCluster == x & PredictedCluster == x, "True Positive",
                        ifelse(TrueCluster != x & PredictedCluster != x, "True Negative", 
                               ifelse(TrueCluster == x & PredictedCluster != x, "False Negative", "False Positive")))
      )
    table(accuracy_mat$Result, dnn = "Counts") %>%
      data.frame() %>%
      mutate(Cluster = x)
    
  }))
  
  # Add image names 
  stats$Image <- image
  
  return(stats)
  
}

#' Wrapper function to calculate values 
#' @param image_num An integer to represent what tiled images are wanted
#' @param subfolder The folder with the text files
#' @param tag Image tag name
#' @param column_name Name of the colum in the metadata file with the cluster designations
#' @param rm_row Remove first row and column on predicted
calc_wrapper <- function(image_num, subfolder, tag, column_name, rm_row_col = FALSE) {
  
  do.call(rbind, lapply(image_num, function(tile) {
    
    # Get tile name 
    tilename <- Metadata[Metadata$Tile == tile, "Path"] %>% head(1) %>% unlist()
    message(tilename)
    
    # Pull truth and predicted data 
    truth_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT", 
                            paste0(tilename, ".txt"))
    truth <- fread(truth_path)
    predicted_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images", subfolder, 
                                paste0(gsub("_Annotations", "", tilename), tag))
    predicted <- fread(predicted_path)
    truth_counts(truth, predicted, tilename, column_name, rm_row_col)
    
  })) %>% return()
  
}

#########################
## DIMENSION REDUCTION ##
#########################

PCA_KCC <- calc_wrapper(1:10, "Kidney_Tiles/PCA_KCC_TXT", "_PCA_KCC.txt", "KCC.PCA")
fwrite(PCA_KCC, "~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/PCA_KCC_Counts.csv")
tSNE_KCC <- calc_wrapper(1:10, "Kidney_Tiles/tSNE_KCC_TXT", "_tSNE_KCC.txt", "KCC.tSNE")
fwrite(tSNE_KCC, "~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/tSNE_KCC_Counts.csv")
SVD_KCC <- calc_wrapper(1:10, "Kidney_Tiles/SVD_KCC_TXT", "_SVD_KCC.txt", "KCC.SVD")
fwrite(SVD_KCC, "~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/SVD_KCC_Counts.csv")

##########
## BLUR ##
##########

# K-Means-----------------------------------------------------------------------

# Non-Blurred
KMeans_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/KMeans_TXT",
                              "_KMeans.txt", "Kmeans")
fwrite(KMeans_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KMeans_Counts.csv", quote = F, row.names = F)

# Blurred 
KMeans_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/KMeans_Blur_TXT",
                                   "_KMeans.txt", "Kmeans.Blur")
fwrite(KMeans_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KMeans_Blur_Counts.csv", quote = F, row.names = F)

# KCC---------------------------------------------------------------------------

# Non-Blurred
KCC_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/KCC_TXT",
                           "_KCC.txt", "KCC")
fwrite(KCC_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KCC_Counts.csv", quote = F, row.names = F)

# Blurred 
KCC_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/KCC_Blur_TXT",
                                "_KCC.txt", "KCC.Blur")
fwrite(KCC_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KCC_Blur_Counts.csv", quote = F, row.names = F)

# Clara-------------------------------------------------------------------------

# Non-Blurred
Clara_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Clara_TXT",
                             "_Clara.txt", "Clara")
fwrite(Clara_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Clara_Counts.csv", quote = F, row.names = F)

# Blurred 
Clara_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Clara_Blur_TXT",
                                "_Clara.txt", "Clara.Blur")
fwrite(Clara_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Clara_Blur_Counts.csv", quote = F, row.names = F)

# Supercells--------------------------------------------------------------------

# Non-Blurred
Scells_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Supercells_TXT",
                             "_supercells.txt", "Supercells")
fwrite(Scells_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Supercell_Counts.csv", quote = F, row.names = F)

# Blurred
Scells_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Supercells_Blur_TXT",
                              "_supercells.txt", "Supercells.Blur")
fwrite(Scells_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Supercell_Blur_Counts.csv", quote = F, row.names = F)

# Recolorize--------------------------------------------------------------------

# Non-Blurred
Re_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Recolorize_TXT",
                              "_recolorize.txt", "Recolorize")
fwrite(Re_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Recolorize_Counts.csv", quote = F, row.names = F)

# Blurred
Re_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/Recolorize_Blur_TXT",
                                   "_recolorize.txt", "Recolorize.Blur")
fwrite(Re_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Recolorize_Blur_Counts.csv", quote = F, row.names = F)

# PyTorch-----------------------------------------------------------------------

# Non-Blurred
PyTorch_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/PyTorch_TXT", 
                               ".txt", "PyTorch")
fwrite(PyTorch_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyTorch_Counts.csv", quote = F, row.names = F)

# Blurred
PyTorch_Blur_Counts <- calc_wrapper(c(3:5, 10, 12, 14, 16, 20, 22, 27), "Kidney_Tiles/PyTorch_Blur_TXT", 
                                    ".txt", "PyTorch.Blur")
fwrite(PyTorch_Blur_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyTorch_Blur_Counts.csv", quote = F, row.names = F)

################
## FULL STUDY ##
################

# K-Means-----------------------------------------------------------------------
KMeans <- calc_wrapper(1:30, "Kidney_Tiles/KMeans_TXT", "_KMeans.txt", "Kmeans") # No image number
fwrite(KMeans, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/KMeans_Counts.csv", quote = F, row.names = F)

# KCC---------------------------------------------------------------------------
KCC <- calc_wrapper(1:30, "Kidney_Tiles/KCC_TXT", "_KCC.txt", "KCC") # 
fwrite(KCC, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/KCC_Counts.csv", quote = F, row.names = F)

# Clara-------------------------------------------------------------------------
Clara <- calc_wrapper(1:30, "Kidney_Tiles/Clara_TXT", "_Clara.txt", "Clara")
fwrite(Clara, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/Clara_Counts.csv", quote = F, row.names = F)

# Supercells--------------------------------------------------------------------
Supercells <- calc_wrapper(1:30, "Kidney_Tiles/Supercells_TXT", "_supercells.txt", "Supercells")
fwrite(Supercells, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/Supercells_Counts.csv", quote = F, row.names = F)

# Recolorize-------------------------------------------------------------------
Recolorize <- calc_wrapper(1:30, "Kidney_Tiles/Recolorize_TXT", "_recolorize.txt", "Recolorize")
fwrite(Recolorize, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/Recolorize_Counts.csv", quote = F, row.names = F)

# PyTorch-----------------------------------------------------------------------
PyTorch <- calc_wrapper(1:30, "Kidney_Tiles/PyTorch_TXT", ".txt", "PyTorch")
fwrite(PyTorch, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/PyTorch_Counts.csv", quote = F, row.names = F)

# Multi-Otsu--------------------------------------------------------------------
MultiOtsu <- calc_wrapper(1:30, "Kidney_Tiles/Multiotsu_TXT", "_multiotsu.txt", "MultiOtsu", TRUE)
fwrite(MultiOtsu, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/MultiOtsu.csv", quote = F, row.names = F)

# Binning-----------------------------------------------------------------------
Binning <- calc_wrapper(1:30, "Kidney_Tiles/Binning_TXT", "_binning.txt", "Binning", TRUE)
fwrite(Binning, "~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/Binning.csv", quote = F, row.names = F)

##########
## ROOT ##
##########

#' Wrapper function to calculate values 
#' @param image_num An integer to represent what tiled images are wanted
#' @param subfolder The folder with the text files
#' @param tag Image tag name
#' @param column_name Name of the column in the metadata file with the cluster designations
#' @param rm_row Remove first row and column on predicted
calc_wrapper_root <- function(image_num, subfolder, tag, column_name, rm_row_col = FALSE) {
  
  do.call(rbind, lapply(image_num, function(tile) {
    
    # Get tile name 
    tilename <- Metadata[Metadata$Tile == tile, "Path"] %>% head(1) %>% unlist()
    message(tilename)
    
    # Pull truth and predicted data 
    truth_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Manual_Segmentation_Masks_TXT/", 
                            paste0(tilename, ".txt"))
    truth <- fread(truth_path)
    predicted_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images", subfolder, 
                                paste0(gsub("_Annotations", "", tilename), tag))
    predicted <- fread(predicted_path)
    truth_counts(truth, predicted, tilename, column_name, rm_row_col)
    
  })) %>% return()
  
}

# Binning-----------------------------------------------------------------------
Binning <- calc_wrapper_root(1:25, "Root/Binning_TXT", "_binning.txt", "Binning", TRUE)
fwrite(Binning, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/Binning_Counts.csv", quote = F, row.names = F)

# Clara-------------------------------------------------------------------------
Clara <- calc_wrapper_root(1:25, "Root/Clara_TXT", "_CLARA.txt", "Clara", TRUE)
fwrite(Clara, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/Clara_Counts.csv", quote = F, row.names = F)

# KCC---------------------------------------------------------------------------
KCC <- calc_wrapper_root(1:25, "Root/KCC_TXT", "_KCC.txt", "KCC", TRUE)
fwrite(KCC, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/KCC_Counts.csv", quote = F, row.names = F)

# KMeans------------------------------------------------------------------------
KMeans <- calc_wrapper_root(1:25, "Root/KMeans_TXT", "_KMeans.txt", "Kmeans", TRUE)
fwrite(KMeans, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/KMeans_Counts.csv", quote = F, row.names = F)

# Multi-Otsu--------------------------------------------------------------------
MultiOtsu <- calc_wrapper_root(1:25, "Root/Multiotsu_TXT", "_multiotsu.txt", "Multiotsu", TRUE)
fwrite(MultiOtsu, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/MultiOtsu_Counts.csv", quote = F, row.names = F)

# pytorch-tip-------------------------------------------------------------------
Pytorch <- calc_wrapper_root(1:25, "Root/PyTorch_TXT", ".txt", "Pytorch", TRUE)
fwrite(Pytorch, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/PyTorch_Counts.csv", quote = F, row.names = F)

# Recolorize--------------------------------------------------------------------
Recolorize <- calc_wrapper_root(1:25, "Root/Recolorize_TXT", "_recolorize.txt", "Recolorize", TRUE)
fwrite(Recolorize, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/Recolorize_Counts.csv", quote = F, row.names = F)

# Supercells--------------------------------------------------------------------
Supercells <- calc_wrapper_root(1:25, "Root/Supercells_TXT", "_supercells.txt", "Supercells", TRUE)
fwrite(Supercells, "~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts/Supercells_Counts.csv", quote = F, row.names = F)



