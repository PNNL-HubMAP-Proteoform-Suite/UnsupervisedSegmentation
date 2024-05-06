# Data maintenance packages 
library(tidyverse)
library(data.table)

Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

#' @param truth A data.frame with cluster values per height (rows) and width (columns) 
#'     for the truth data 
#' @param predicted A data.frame with cluster values per height (rows) and width (columns)
#'     for the predicted values. Must be oriented the same way as truth. 
#' @param image The name of the image in the metadata file
#' @param model The name of the model column in the metadata file 
truth_counts <- function(truth, predicted, image, model) {
  
  # Pull the number of clusters
  NumClusters <- max(truth)
  
  # Make the submeta data.frame and the cluster conversion
  SubMeta <- Metadata %>% filter(Path == image)
  ClusterConvert <- SubMeta[[model]]
  names(ClusterConvert) <- SubMeta$ManualClusterNumber
  
  # Make function to pivot image data.frames
  make_pivot <- function(df) {
    df %>%
      mutate(Height = 1:nrow(.)) %>%
      pivot_longer(cols = 1:(ncol(.)-1)) %>%
      rename(Width = name, Cluster = value) %>%
      mutate(Width = gsub("V", "", Width) %>% as.numeric())
  }
  
  # Merge data.frames for counts 
  toCalc <- left_join(
    make_pivot(truth) %>% rename(TrueCluster = Cluster),
    make_pivot(predicted) %>% mutate(Cluster = map_int(Cluster, function(x) {ClusterConvert[[x]]})) %>% rename(PredictedCluster = Cluster),
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

# K-Means-----------------------------------------------------------------------

KMeans_Counts <- do.call(rbind, lapply(1:30, function(tile) {
  
  # Get tile name 
  tilename <- Metadata[Metadata$Tile == tile, "Path"] %>% head(1) %>% unlist()
  
  message(tilename)
  
  # Read data
  truth_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT", 
                          paste0(tilename, ".txt"))
  truth <- fread(truth_path)
  predicted_path <- file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                              paste0(gsub("_Annotations", "", tilename), "_KMeans.txt"))
  predicted <- fread(predicted_path)
  
  truth_counts(truth, predicted, tilename, "KMeans")
  
  
}))

fwrite(KMeans_Counts, "~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KMeans_Counts.csv", quote = F, row.names = F)



















