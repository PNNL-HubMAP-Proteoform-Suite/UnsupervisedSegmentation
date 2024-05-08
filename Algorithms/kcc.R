#' @param in_path Path to the input image 
#' @param out_path_data Path to place the segmented image data.frame
#' @param out_path_image Path to place the segmented image 
#' @param k Number of clusters 
apply_kcc <- function(in_path, k, out_path) {
  
  # Image processing libraries
  library(flexclust)
  library(magick)
  library(png)
  
  # Data processing libraries 
  library(tidyverse)
  library(data.table)
  
  # Create unique ID
  library(uuid)
  id <- UUIDgenerate()
  
  # Blur the image 
  img <- image_read(in_path)
  blurred <- image_blur(img, radius = 100, sigma = 10)
  path <- tempdir()
  image_write(blurred, file.path(path, id), format = "png")
  
  # Read image 
  imgRead <- readPNG(file.path(path, id))
  
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
  KCC <- kcca(Img_DF[,c("Red", "Green", "Blue")], k = k)
  KCentroid <- Img_DF %>% mutate(Cluster = as.factor(KCC@cluster))
  
  # Shrink size
  Smaller <- KCentroid %>%
    mutate(X = factor(X, levels = 1:max(X))) %>%
    pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
    arrange(Y) %>%
    select(-Y)
  colnames(Smaller) <- paste0("V", colnames(Smaller))
  
  # Write file
  end_string <- strsplit(in_path, "/") %>% unlist() %>% tail(1) %>% gsub(pattern = ".png", replacement = "_KCC.txt", fixed = T)
  fwrite(Smaller, file.path(out_path, end_string), quote = F, row.names = F, sep = "\t")
  
}


