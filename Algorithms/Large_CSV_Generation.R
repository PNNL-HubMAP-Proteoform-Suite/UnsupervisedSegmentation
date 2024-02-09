# Data maintenance packages 
library(tidyverse)
library(png)
library(flexclust) #KCC
library(M3C) #t-SNE
library(e1071) #t-SNE
library(SuperpixelImageSegmentation)
library(OpenImageR)
library(ClusterR)
library(glue)


# This file was used to generate the CSVs for all of the segmenation models other than pyImSeg and 
# PyTorch-tip

# Convert pixels to a data.frame
convert_df <- function(the_mat, the_name) {
  the_mat %>% 
    data.frame() %>% 
    `colnames<-`(paste0("X", 1:ncol(the_mat))) %>%
    pivot_longer(1:ncol(the_mat)) %>%
    rename(X = name) %>%
    mutate(Y = rep(paste0("Y", nrow(the_mat):1), each = ncol(the_mat))) %>%
    relocate(X) %>%
    rename(!!the_name := value) }

image_dataframe = function(im_mat){
  im_mat %>% 
    data.frame() %>% 
    `colnames<-`(paste0(1:ncol(im_mat))) %>%
    pivot_longer(1:ncol(im_mat)) %>% 
    mutate(Y = rep(paste0(nrow(im_mat):1), each = ncol(im_mat))) %>% 
    rename(X = name) %>% 
    mutate(X = as.integer(X), Y = as.integer(Y), cluster = as.character(value)) %>% 
    select(-value)
}
  
  
  main = function(image_path, cluster_num){
    
    image_name = substr(image_path, 100, 115)
    
    im = OpenImageR::readImage(image_path)
    
    init = Image_Segmentation$new()
    
    # slic
    slic = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                    kmeans_method = "",
                                    superpixel = 35, 
                                    AP_data = TRUE,
                                    use_median = FALSE, 
                                    sim_wL = 3, 
                                    sim_wA = 1, 
                                    sim_wB = 1,
                                    sim_color_radius = 1, 
                                    verbose = TRUE, 
                                    return_labels_2_dimensionsional = TRUE)

    
    # slico
    slico = init$spixel_segmentation(input_image = im, #pretrained forest structure model
                                     method = "slico",
                                     kmeans_method = "",
                                     superpixel = 50, 
                                     AP_data = TRUE,
                                     use_median = FALSE, 
                                     sim_wL = 3, 
                                     sim_wA = 10, 
                                     sim_wB = 10,
                                     sim_color_radius = 1, 
                                     verbose = TRUE, 
                                     return_labels_2_dimensionsional = TRUE)
    

  testImg = readPNG(image_path)
    
  Img_DF <- Reduce(left_join, list(convert_df(testImg[,,1], "Red"),
                                     convert_df(testImg[,,2], "Green"),
                                     convert_df(testImg[,,3], "Blue")))
  Img_DF$X <- gsub("X", "", Img_DF$X) %>% as.numeric()
  Img_DF$Y <- gsub("Y", "", Img_DF$Y) %>% as.numeric()

  KMeans <- Img_DF %>% mutate(Cluster = as.factor(kmeans(Img_DF[,c("Red", "Green", "Blue")], centers = cluster_num)$cluster))
  
  KCC <- kcca(Img_DF[,c("Red", "Green", "Blue")], k = cluster_num)
  KCentroid <- Img_DF %>% mutate(Cluster = as.factor(KCC@cluster))
  
  pca <- princomp(Img_DF[,c("Red", "Green", "Blue")])
  PCA_KM <- Img_DF %>% mutate(Cluster = as.factor(kmeans(pca$scores, cluster_num)$cluster)) 
  
  uniqColors <- unique(Img_DF[,c("Red", "Green", "Blue")])
  TSN <- tsne(t(uniqColors))
  TSN_UC <- uniqColors %>% mutate(Cluster = kmeans(TSN$data, cluster_num)$cluster)
  
  # Merge data
  TSN_KM <- left_join(Img_DF, TSN_UC, by = c("Red", "Green", "Blue")) %>% mutate(Cluster = as.factor(Cluster))
  
  
  # Saving CSVs #
  
  slicDF = image_dataframe(slic$spix_labels)
  write.csv(slicDF, glue("/path/to/csv"), row.names = FALSE)
  
  slicoDF = image_dataframe(slico$spix_labels)
  write.csv(slicoDF, glue("/path/to/csv"), row.names = FALSE)
  
  # KMeans
  write.csv(KMeans, glue("/path/to/csv"), row.names = FALSE)
  
  #KCC
  write.csv(KCentroid, glue("/path/to/csv"), row.names = FALSE)
  
  #PCA_KM
  write.csv(PCA_KM, glue("/path/to/csv"), row.names = FALSE)
  
  #TSN_KM
  write.csv(TSN_KM, glue("/path/to/csv"), row.names = FALSE)
  }
  
  
  
directory = "/file/to/png/folder/"
setwd(directory)
  
summary.df = read.csv2("/path/to/summary/file", sep = ",")
  
file.png =
    list.files(
      directory,
      pattern = "*.png",
      ignore.case = TRUE,
      full.names = TRUE)
counter = 0

for (path in file.png) {
  counter = counter + 1
  main(path, as.numeric(summary.df$Total.Feature.Classes[counter]))
  print(counter)
 }