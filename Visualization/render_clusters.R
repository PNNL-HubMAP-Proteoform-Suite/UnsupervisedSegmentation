library(data.table)
library(tidyverse)

render_cluster <- function(data, colors, order) {
  
  colorList <- colors
  names(colorList) <- order
  
  data %>%
    mutate(Height = 1:nrow(.)) %>%
    pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
    rename(Cluster = value, Width = name) %>%
    mutate(Width = gsub("V", "", Width) %>% as.numeric(),
           Cluster = as.factor(Cluster)) %>% 
    ggplot(aes(x = Width, y = Height, fill = Cluster)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_manual(values = colorList) +
    theme_void() +
    theme(legend.position = "none")
  
}

## Clusters need to be matched manually. Matches are tracked in the Annotation Summary csv

# KMeans------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

lapply(1:30, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                          gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Kmeans]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG", 
                   gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})


# KCC---------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")

lapply(3, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT/", 
                          gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, KCC]))
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", 
                   gsub("_Annotations", "_KCC.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

lapply(3, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/", 
                          gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, KCC.Blur]))
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", 
                   gsub("_Annotations", "_KCC.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})


# Clara-------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 1
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_TXT/", 
                        gsub(pattern = "Annotations", replacement = "Clara.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, Clara.Blur]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/", 
                 gsub("_Annotations", "_Clara.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# Superpixels-------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")

lapply(1, function(subtile) {
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_TXT/", 
                          gsub(pattern = "Annotations", replacement = "supercells.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Supercells]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", 
                   gsub("_Annotations", "_supercells.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})


lapply(1, function(subtile) {
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_TXT/", 
                          gsub(pattern = "Annotations", replacement = "supercells.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Supercells.Blur]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_PNG/", 
                   gsub("_Annotations", "_supercells.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

# Recolorize--------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")

lapply(1, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_TXT/", 
                          gsub(pattern = "Annotations", replacement = "recolorize.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Recolorize]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", 
                   gsub("_Annotations", "_recolorize.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})

lapply(1, function(subtile) {

  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_TXT/", 
                          gsub(pattern = "Annotations", replacement = "recolorize.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Recolorize.Blur]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_PNG/", 
                   gsub("_Annotations", "_recolorize.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})


