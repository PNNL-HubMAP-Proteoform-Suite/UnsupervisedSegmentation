library(tidyverse)
library(data.table)
library(gtools)

assign_cluster <- function(truth, predicted) {
  
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
    make_pivot(predicted) %>% rename(PredictedCluster = Cluster),
    by = c("Height", "Width")
  )
  
  # Get every arrangement of clusters 
  k <- max(toCalc$PredictedCluster)
  arr <- permutations(k, k, v = 1:k)
  message(paste("...The number of rows is", nrow(arr)))
  
  # Get a balanced accuracy per arrangement
  BA_Values <- lapply(1:nrow(arr), function(row) {
    
    if (row %% 5 == 0) {message(paste("...on row", row))}
    
    # Make the test dataframe
    test <- toCalc

    # Make replacements    
    mylist <- arr[row,]
    names(mylist) <- 1:k
    
    # Apply replacements
    test <- test %>% mutate(PredictedCluster = map_int(PredictedCluster, function(x) {mylist[[x]]}))
    
    # Conduct stats per clusters
    do.call(rbind, lapply(1:k, function(x) {
      
      accuracy_mat <- test %>%
        mutate(
          Result = ifelse(TrueCluster == x & PredictedCluster == x, "True Positive",
                   ifelse(TrueCluster != x & PredictedCluster != x, "True Negative", 
                   ifelse(TrueCluster == x & PredictedCluster != x, "False Negative", "False Positive")))
        )
      table(accuracy_mat$Result, dnn = "Counts") %>%
        data.frame() %>%
        mutate(Cluster = x)
      
    })) %>% 
      pivot_wider(id_cols = Cluster, names_from = Counts, values_from = Freq) %>%
      mutate(
        TPR = `True Positive` / (`True Positive` + `False Negative`),
        TNR = `True Negative` / (`True Negative` + `False Positive`)
      ) %>%
      group_by(Cluster) %>%
      mutate(BA = mean(TPR, TNR), na.rm = T) %>%
      ungroup() %>%
      select(BA) %>%
      unlist() %>%
      mean(na.rm = T) %>%
      return()
    
  }) %>% unlist()
  
  message(paste("...The max BA was", max(BA_Values)))
  
  # Fix the clusters to the new order
  true_order <- arr[which.max(BA_Values),]
  return(true_order)
  
}

metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

#############
## K-MEANS ##
#############

correct_order <- lapply(2:30, function(num) {
  path <- unique(metadata$Path)[num]
  path <- gsub("_Annotations", "", path, fixed = T)
  assign_cluster(
    truth = fread(paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/", path, "_Annotations.txt")),
    predicted = fread(paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT/", path, "_KMeans.txt"))
  )
})







