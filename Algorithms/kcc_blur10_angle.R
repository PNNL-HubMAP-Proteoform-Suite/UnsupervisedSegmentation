library(doParallel)
library(foreach)

library(magick)

blur <- function(in_path, out_path, radius = 100, sigma = 10) {
  img <- image_read(in_path)
  blurred <- image_blur(img, radius = radius, sigma = sigma)
  image_write(blurred, out_path)
}

# Blur all the images 
library(tidyverse)


kcc <- function(in_path, out_path_data, out_path_image, k, family) {
  
  library(flexclust) 
  library(png)
  
  library(tidyverse)
  library(data.table)
  
  # Read all layers of an image 
  imgRead <- png::readPNG(in_path)
  
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
  
  # Run clustering 
  KCC <- kcca(Img_DF[,c("Red", "Green", "Blue")], k = k, family = family)
  KCentroid <- Img_DF %>% mutate(Cluster = as.factor(KCC@cluster))
  
  # Save plot 
  clusPlot <- ggplot(KCentroid, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_void()
  
  # Write results
  fwrite(KCentroid %>% dplyr::select(X, Y, Cluster), out_path_data, quote = F, row.names = F, sep = "\t")
  ggsave(out_path_image, clusPlot)
  
}

# Get a number of clusters 
Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")



