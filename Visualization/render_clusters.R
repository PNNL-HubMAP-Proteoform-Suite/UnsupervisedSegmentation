library(data.table)
library(tidyverse)

render_cluster <- function(data, colors, order) {
  
  colors <- colors[order]
  
  data %>%
    mutate(Height = 1:nrow(.)) %>%
    pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
    rename(Cluster = value, Width = name) %>%
    mutate(Width = gsub("V", "", Width) %>% as.numeric(),
           Cluster = as.factor(Cluster)) %>% 
    ggplot(aes(x = Width, y = Height, fill = Cluster)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(legend.position = "none")
  
}

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")


# KMeans------------------------------------------------------------------------

lapply(unique(Image_Metadata$Tile), function(tile) {
  
  root <- unique(Image_Metadata$Path)[tile]
  
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                          gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, KMeans]))
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG", 
                   gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})




