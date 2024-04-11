#' @param in_path Path to the input image 
#' @param out_path_data Path to place the segmented image data.frame
#' @param out_path_image Path to place the segmented image 
#' @param k Number of clusters 
tsne_kcc <- function(in_path, out_path_data, out_path_image, k) {
  
  # Image processing libraries
  library(flexclust)
  library(magick)
  library(png)
  
  # Data processing libraries 
  library(tidyverse)
  library(data.table)
  
  # Add pacakges for tsne
  library(M3C) # BiocManager::install("M3C")
  library(e1071)
  
  # Read image
  img <- image_read(in_path)
  imgRead <- readPNG(in_path)
  
  # Run a function for converting the data.frame 
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
  
  # Generate an image data.frame to perform statistics on 
  Img_DF <- Reduce(left_join, list(convert_df(imgRead[,,1], "Red"),
                                   convert_df(imgRead[,,2], "Green"),
                                   convert_df(imgRead[,,3], "Blue")))
  Img_DF$X <- gsub("X", "", Img_DF$X) %>% as.numeric()
  Img_DF$Y <- gsub("Y", "", Img_DF$Y) %>% as.numeric()
  
  # Do t-sne on the transpose of the data.frame
  uniqColors <- unique(Img_DF[,c("Red", "Green", "Blue")])
  TSN <- tsne(t(uniqColors))
  
  # Run clustering
  TSN_UC <- uniqColors %>% mutate(Cluster = kmeans(TSN$data, k)$cluster)
  TSN_KM <- left_join(Img_DF, TSN_UC, by = c("Red", "Green", "Blue")) %>% mutate(Cluster = as.factor(Cluster))

  # Save plot 
  clusPlot <- ggplot(TSN_KM, aes(x = X, y = Y, fill = Cluster)) + geom_raster(interpolate = TRUE) + 
    theme_void() +
    scale_fill_brewer(palette = "Spectral") + theme(legend.position = "none")
  
  # Write results
  fwrite(TSN_KM %>% 
           dplyr::select(X, Y, Cluster) %>%
           pivot_wider(values_from = Cluster, id_cols = X, names_from = Y), out_path_data, quote = F, row.names = F, sep = "\t")
  ggsave(out_path_image, clusPlot, units = "px", width = image_info(img)$width, height = image_info(img)$height)
  
}



