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

# KMeans Blur-------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_TXT", 
                        gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, Kmeans.Blur]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_PNG", 
                 gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))



# KMeans------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
tile <- 30
root <- unique(Image_Metadata$Path)[tile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                        gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KMeans]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG", 
                 gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# KCC---------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT", 
                        gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KCC]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG", 
                 gsub("_Annotations", "_KCC.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))

# KCC Blur----------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X")
subtile <- 10
root <- unique(Image_Metadata$Path)[subtile]
data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/", 
                        gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
plot <- render_cluster(data,
                       unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                       unlist(Image_Metadata[Image_Metadata$Path == root, KCC.Blur]))
plot

ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", 
                 gsub("_Annotations", "_KCC.png", root)),  plot = plot,
       units = "px", height = nrow(data), width = ncol(data))








