library(magick)
library(tidyverse)
library(data.table)

#' @description Reads an svg of annotations on an H&E image
#' @param image_path Path to the svg
#' @param Features A data.frame with the feature name "symbol", R G B values "red"
#'     "green" "blue", and a pixel threshold "thresh" in RGB units. 
SVG_to_CSV <- function(image_path,
                       Features) { 

  svg <- image_read(image_path)
  img <- image_data(svg)
  
  make_df <- function(layer) {
    
    img[layer,,] %>%
      data.frame() %>%
      mutate(Length = 1:nrow(.)) %>%
      relocate(Length) %>%
      pivot_longer(2:ncol(.)) %>%
      rename(Width = name) %>%
      mutate(Width = gsub("X", "", Width) %>% as.numeric()) %>%
      return()
    
  }
  
  ProcessedImage <- make_df(1) %>%
    rename(R = value) %>%
    mutate(
      G = make_df(2)$value,
      B = make_df(3)$value,
      RGB = paste0("#", R, G, B)
    ) 
  
  ColorConverter <- ProcessedImage %>%
    select(RGB) %>%
    unique() %>%
    mutate(
      Red = lapply(RGB, function(x) {unlist(col2rgb(x)["red", 1])}) %>% unlist(),
      Green = lapply(RGB, function(x) {unlist(col2rgb(x)["green", 1])}) %>% unlist(),
      Blue = lapply(RGB, function(x) {unlist(col2rgb(x)["blue", 1])}) %>% unlist(),
      Cluster = "Background"
    )
  
  nada <- lapply(1:nrow(Features), function(row) {
    
    ColorConverter[
      ColorConverter$Red >= Features$red[row] - Features$thresh[row] &
      ColorConverter$Red <= Features$red[row] + Features$thresh[row] &
      ColorConverter$Green >= Features$green[row] - Features$thresh[row] &
      ColorConverter$Green <= Features$green[row] + Features$thresh[row] &
      ColorConverter$Blue >= Features$blue[row] - Features$thresh[row] &
      ColorConverter$Blue <= Features$blue[row] + Features$thresh[row], "Cluster" 
    ] <<- Features$symbol[row]
    
  })
  rm(nada)
  
  ProcessedImage <- left_join(ProcessedImage, ColorConverter[, c("RGB", "Cluster")], by = "RGB") %>%
    mutate(Width = max(Width) - Width + 1) %>%
    select(-c(R,G,B,RGB)) %>%
    mutate(Cluster = as.factor(as.numeric(as.factor(Cluster)))) %>%
    rename(Height = Width, Width = Length)
  
  return(ProcessedImage)
  
}

make_plot <- function(ProcessedImage, ColorScale) {
  ggplot(ProcessedImage, aes(x = Width, y = Height, fill = Cluster)) +
    geom_raster(interpolate = TRUE) +
    theme_void() +
    scale_fill_manual(values = ColorScale) +
    theme(legend.position = "none")
}

format_mask <- function(ProcessedImage) {
  matrix(ProcessedImage$Cluster, nrow = max(ProcessedImage$Height), ncol = max(ProcessedImage$Width))
}

## Make List of Features ## 
Features <- data.frame(
  symbol = c("Green", "Green", "Pink", "Pink", "Purple", "Purple", "Red", "Red", "Yellow", "Yellow"),
  red = c(55, 195, 220, 244, 70, 199, 234, 249, 208, 236),
  green = c(126, 216, 171, 230, 8, 180, 51, 193, 207, 235),
  blue = c(34, 188, 22, 245, 27, 186, 35, 189, 111, 197),
  thresh = c(30, 30, 10, 10, 30, 30, 30, 30, 20, 20)
)

## Pull SVGs ##

# Tile 1------------------------------------------------------------------------
Mask1 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X001Y006_Annotations.svg", 
                   Features %>% filter(symbol %in% c("Green", "Pink", "Purple", "Red")))
Img1 <- make_plot(Mask1, c("white", "green", "pink", "purple", "red"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y006_Annotations.png", plot = Img1,
       units = "px", height = max(Mask1$Height), width = max(Mask1$Width))
write.table(format_mask(Mask1), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y006_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 2------------------------------------------------------------------------
Mask2 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X001Y007_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Purple")))
Img2 <- make_plot(Mask2, c("white", "green", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y007_Annotations.png", plot = Img2,
       units = "px", height = max(Mask2$Height), width = max(Mask2$Width))
write.table(format_mask(Mask2), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y007_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 3------------------------------------------------------------------------
Mask3 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X001Y008_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Red")))
Img3 <- make_plot(Mask3, c("white", "green", "pink", "red"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y008_Annotations.png", plot = Img3,
       units = "px", height = max(Mask3$Height), width = max(Mask3$Width))
write.table(format_mask(Mask3), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y008_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 4------------------------------------------------------------------------
Mask4 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X001Y009_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Red", "Yellow", "Purple")))
Img4 <- make_plot(Mask4, c("white", "green", "pink", "purple" "red", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y008_Annotations.png", plot = Img3,
       units = "px", height = max(Mask3$Height), width = max(Mask3$Width))
write.table(format_mask(Mask3), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y008_Annotations.txt",
            quote = F, row.names = F, sep = "\t")







