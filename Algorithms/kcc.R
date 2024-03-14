library(doParallel)
library(foreach)

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

# Process blur images 
blur_df <- data.frame(PostBlur = list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Blur", full.names = T)) %>%
  mutate(
    Mask = gsub("Blur/", "KCC_Blur_Angle/Mask/", PostBlur) %>% gsub(pattern = ".png", replacement = "_KCC_angle.txt", ., fixed = T),
    ResultImage = gsub("Blur/", "KCC_Blur_Angle/Image/", PostBlur) %>% gsub(pattern = ".png", replacement = "_KCC_angle.png", ., fixed = T),
    Clusters = Metadata$`Total Feature Classes`
  )


cl <- makeCluster(8)
registerDoParallel(cl)

foreach(x = 17:nrow(blur_df)) %dopar% { 
  
  kcc(
    in_path = blur_df$PostBlur[x],
    out_path_data = blur_df$Mask[x],
    out_path_image = blur_df$ResultImage[x],
    k = blur_df$Clusters[x],
    family = kccaFamily("angle")
  )
  
}

stopCluster(cl)



