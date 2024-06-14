#' @param in_path Path to the input image 
#' @param k Number of clusters
#' @param out_path Path to place the segmented image data.frame
#' @param blur A boolean (TRUE/FALSE) to indicate whether the image should be blurred or not 
apply_recolorize <- function(in_path, k, out_path, blur) {
  
  # Image processing libraries
  library(png)
  library(magick)
  
  # Data processing libraries 
  library(tidyverse)
  library(data.table)
  library(uuid)
  library(recolorize)
  
  # If blur, make and read the blurred image 
  if (blur) {
    
    # Generate unique name
    id <- paste0(UUIDgenerate(), ".png")
    
    # Blur the image
    img <- image_read(in_path)
    blurred <- image_blur(img, radius = 100, sigma = 10)
    path <- tempdir()
    image_write(blurred, file.path(path, id), format = "png")
    
    # Read blurred image
    oripath <- in_path
    in_path <- file.path(path, id)
    
  } 
  
  # Define groups 
  groups <- recolorize(in_path, method = "histogram", bins = k)$pixel_assignments
  
  # Assign group sizes
  sizes <- table(groups) %>%
    data.frame() %>%
    arrange(-Freq) %>%
    mutate(
      Cluster = 1:nrow(.),
      Cluster = ifelse(Cluster > k, k, Cluster)
    ) %>%
    rename(value = groups)
  
  # Make clusters
  Recolor <- groups %>%
    data.frame() %>%
    mutate(Y = 1:nrow(.)) %>%
    pivot_longer(cols = c(1:(ncol(.)-1))) %>%
    rename(X = name) %>%
    mutate(
      X = gsub("X", "", X) %>% as.numeric(),
      Y = max(Y) - Y + 1,
      value = as.factor(value)
    ) %>%
    left_join(sizes %>% select(value, Cluster))
  
  # Shrink size
  Smaller <- Recolor %>%
    mutate(X = factor(X, levels = 1:max(X))) %>%
    pivot_wider(id_cols = Y, names_from = X, values_from = Cluster) %>%
    arrange(Y) %>%
    select(-Y)
  colnames(Smaller) <- paste0("V", colnames(Smaller))
  
  # Write file
  if (blur) {
    end_string <- strsplit(oripath, "/") %>% unlist() %>% tail(1) %>% gsub(pattern = ".png", replacement = "_recolorize.txt", fixed = T)
  } else {
    end_string <- strsplit(in_path, "/") %>% unlist() %>% tail(1) %>% gsub(pattern = ".png", replacement = "_recolorize.txt", fixed = T)
  }
  fwrite(Smaller, file.path(out_path, end_string), quote = F, row.names = F, sep = "\t")
  
}