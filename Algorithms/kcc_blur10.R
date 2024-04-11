#' @param in_path Path to the input image 
#' @param out_path_data Path to place the segmented image data.frame
#' @param out_path_image Path to place the segmented image 
#' @param k Number of clusters 
kcc_blur10_angle <- function(in_path, out_path_data, out_path_image, k) {
  
  # Image processing libraries
  library(flexclust)
  library(magick)
  library(png)
  
  # Data processing libraries 
  library(tidyverse)
  library(data.table)
  
  # Blur the image 
  img <- image_read(in_path)
  blurred <- image_blur(img, radius = 100, sigma = 10)
  path <- tempdir()
  image_write(blurred, file.path(path, "image.jpg"))
  
  # Read image 
  imgRead <- readPNG(file.path(path, "image.jpg"))
  
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
  
  # Save plot 
  clusPlot <- ggplot(KCentroid, aes(x = X, y = Y, fill = Cluster)) + geom_raster(interpolate = TRUE) + 
    theme_void() +
    scale_fill_brewer(palette = "Spectral") + theme(legend.position = "none")
  
  # Write results
  fwrite(KCentroid %>% 
           dplyr::select(X, Y, Cluster) %>%
           pivot_wider(values_from = Cluster, id_cols = X, names_from = Y), out_path_data, quote = F, row.names = F, sep = "\t")
  dim <- magick::image_attributes(img)[9, "value"] %>%
    strsplit(",") %>%
    unlist() %>%
    as.numeric()
  ggsave(out_path_image, clusPlot, units = "px", width = dim[1], height = dim[2])
  
}


