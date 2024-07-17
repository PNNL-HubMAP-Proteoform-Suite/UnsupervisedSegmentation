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
  mat <- matrix(ProcessedImage$Cluster, nrow = max(ProcessedImage$Height), ncol = max(ProcessedImage$Width)) 
  mat[nrow(mat):1,]
}

## Make List of Features ## 
Features <- data.frame(
  symbol = c("Green", "Green", "Pink", "Pink", "Purple", "Purple", "Red", "Red", 
             "Yellow", "Yellow", "Purple", "Gray", "Gray", "Blue", "Blue", "Red2", "Red2",
             "DarkGray"),
  red = c(55, 195, 220, 244, 70, 199, 234, 249, 208, 236, 159, 128, 209, 57, 183, 219, 195, 178),
  green = c(126, 216, 171, 230, 8, 180, 51, 193, 207, 235, 128, 128, 209, 48, 180, 84, 154, 178),
  blue = c(34, 188, 22, 245, 27, 186, 35, 189, 111, 197, 137, 128, 209, 138, 212, 70, 135, 178),
  thresh = c(30, 30, 10, 10, 20, 20, 30, 30, 20, 20, 30, 10, 10, 10, 10, 30, 30, 20)
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
Img4 <- make_plot(Mask4, c("white", "green", "pink", "purple", "red", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y009_Annotations.png", plot = Img4,
       units = "px", height = max(Mask4$Height), width = max(Mask4$Width))
write.table(format_mask(Mask4), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y009_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 5------------------------------------------------------------------------
Mask5 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X001Y010_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Purple", "Yellow")))
Img5 <- make_plot(Mask5, c("white", "green", "pink", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X001Y010_Annotations.png", plot = Img5,
       units = "px", height = max(Mask5$Height), width = max(Mask5$Width))
write.table(format_mask(Mask5), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X001Y010_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 6------------------------------------------------------------------------
Mask6 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y005_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Yellow")))
Img6 <- make_plot(Mask6, c("white", "green", "pink", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y005_Annotations.png", plot = Img6,
       units = "px", height = max(Mask6$Height), width = max(Mask6$Width))
write.table(format_mask(Mask6), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y005_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 7------------------------------------------------------------------------
Mask7 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y006_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Yellow", "Purple")))
Img7 <- make_plot(Mask7, c("white", "green", "pink", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y006_Annotations.png", plot = Img7,
       units = "px", height = max(Mask7$Height), width = max(Mask7$Width))
write.table(format_mask(Mask7), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y006_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 8------------------------------------------------------------------------
Mask8 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y007_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green", "Pink", "Purple")))
Img8 <- make_plot(Mask8, c("white", "green", "pink", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y007_Annotations.png", plot = Img8,
       units = "px", height = max(Mask8$Height), width = max(Mask8$Width))
write.table(format_mask(Mask8), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y007_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 9------------------------------------------------------------------------
Mask9 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y008_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Gray", "Purple")))
Img9 <- make_plot(Mask9, c("white", "gray", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y008_Annotations.png", plot = Img9,
       units = "px", height = max(Mask9$Height), width = max(Mask9$Width))
write.table(format_mask(Mask9), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y008_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 10------------------------------------------------------------------------
Mask10 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y009_Annotations.svg", 
                    Features %>% filter(symbol %in% c("Green")))
Img10 <- make_plot(Mask10, c("white", "green"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y009_Annotations.png", plot = Img10,
       units = "px", height = max(Mask10$Height), width = max(Mask10$Width))
write.table(format_mask(Mask10), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y009_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 11------------------------------------------------------------------------
Mask11 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y010_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Red2")))
Img11 <- make_plot(Mask11, c("white", "green", "red"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y010_Annotations.png", plot = Img11,
       units = "px", height = max(Mask11$Height), width = max(Mask11$Width))
write.table(format_mask(Mask11), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y010_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 12------------------------------------------------------------------------
Mask12 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y011_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green")))
Img12 <- make_plot(Mask12, c("white", "green"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y011_Annotations.png", plot = Img12,
       units = "px", height = max(Mask12$Height), width = max(Mask12$Width))
write.table(format_mask(Mask12), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y011_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 13------------------------------------------------------------------------
Mask13 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y012_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Pink")))
Img13 <- make_plot(Mask13, c("white", "green", "pink"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y012_Annotations.png", plot = Img13,
       units = "px", height = max(Mask13$Height), width = max(Mask13$Width))
write.table(format_mask(Mask13), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y012_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 14------------------------------------------------------------------------
Mask14 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y013_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple")))
Img14 <- make_plot(Mask14, c("white", "green", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y013_Annotations.png", plot = Img14,
       units = "px", height = max(Mask14$Height), width = max(Mask14$Width))
write.table(format_mask(Mask14), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y013_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 15------------------------------------------------------------------------
Mask15 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y014_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple", "Pink", "Blue")))
Img15 <- make_plot(Mask15, c("white", "blue", "green", "pink", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y014_Annotations.png", plot = Img15,
       units = "px", height = max(Mask15$Height), width = max(Mask15$Width))
write.table(format_mask(Mask15), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y014_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 16------------------------------------------------------------------------
Mask16 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y015_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple", "Pink", "Blue")))
Img16 <- make_plot(Mask16, c("white", "blue", "green", "pink", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y015_Annotations.png", plot = Img16,
       units = "px", height = max(Mask16$Height), width = max(Mask16$Width))
write.table(format_mask(Mask16), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y015_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 17------------------------------------------------------------------------
Mask17 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y016_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple", "Pink", "Blue")))
Img17 <- make_plot(Mask17, c("white", "blue", "green", "pink", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y016_Annotations.png", plot = Img17,
       units = "px", height = max(Mask17$Height), width = max(Mask17$Width))
write.table(format_mask(Mask17), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y016_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 18------------------------------------------------------------------------
Mask18 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y017_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple", "Pink", "Blue")))
Img18 <- make_plot(Mask18, c("white", "blue", "green", "pink", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y017_Annotations.png", plot = Img18,
       units = "px", height = max(Mask18$Height), width = max(Mask18$Width))
write.table(format_mask(Mask18), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y017_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 19------------------------------------------------------------------------
Mask19 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y018_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple", "Blue")))
Img19 <- make_plot(Mask19, c("white", "blue", "green", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y018_Annotations.png", plot = Img19,
       units = "px", height = max(Mask19$Height), width = max(Mask19$Width))
write.table(format_mask(Mask19), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y018_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 20------------------------------------------------------------------------
Mask20 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y019_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Yellow", "Purple", "Gray")))
Img20 <- make_plot(Mask20, c("white", "gray", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y019_Annotations.png", plot = Img20,
       units = "px", height = max(Mask20$Height), width = max(Mask20$Width))
write.table(format_mask(Mask20), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y019_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 21------------------------------------------------------------------------
Mask21 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y020_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Yellow", "Purple", "Gray", "DarkGray")))
Img21 <- make_plot(Mask21, c("white", "black", "gray", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y020_Annotations.png", plot = Img21,
       units = "px", height = max(Mask21$Height), width = max(Mask21$Width))
write.table(format_mask(Mask21), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y020_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 22------------------------------------------------------------------------
Mask22 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y021_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Yellow", "Purple", "Gray", "Blue", "DarkGray")))
Img22 <- make_plot(Mask22, c("white", "blue", "black", "gray", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y021_Annotations.png", plot = Img22,
       units = "px", height = max(Mask22$Height), width = max(Mask22$Width))
write.table(format_mask(Mask22), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y021_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 23------------------------------------------------------------------------
Mask23 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X002Y022_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Yellow", "Gray")))
Img23 <- make_plot(Mask23, c("white", "gray", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y022_Annotations.png", plot = Img23,
       units = "px", height = max(Mask23$Height), width = max(Mask23$Width))
write.table(format_mask(Mask23), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y022_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 24------------------------------------------------------------------------
Mask24 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y003_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Blue")))
Img24 <- make_plot(Mask24, c("white", "blue", "green"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y003_Annotations.png", plot = Img24,
       units = "px", height = max(Mask24$Height), width = max(Mask24$Width))
write.table(format_mask(Mask24), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y003_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 25------------------------------------------------------------------------
Mask25 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y004_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Blue", "Pink", "Yellow")))
Img25 <- make_plot(Mask25, c("white", "blue", "green", "pink", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y004_Annotations.png", plot = Img25,
       units = "px", height = max(Mask25$Height), width = max(Mask25$Width))
write.table(format_mask(Mask25), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y004_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 26------------------------------------------------------------------------
Mask26 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y005_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Yellow")))
Img26 <- make_plot(Mask26, c("white", "green", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y005_Annotations.png", plot = Img26,
       units = "px", height = max(Mask26$Height), width = max(Mask26$Width))
write.table(format_mask(Mask26), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y005_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 27------------------------------------------------------------------------
Mask27 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y006_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Green", "Purple")))
Img27 <- make_plot(Mask27, c("white", "green", "purple"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y006_Annotations.png", plot = Img27,
       units = "px", height = max(Mask27$Height), width = max(Mask27$Width))
write.table(format_mask(Mask27), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y006_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 28------------------------------------------------------------------------
Mask28 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y007_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Purple", "Gray", "Yellow")))
Img28 <- make_plot(Mask28, c("white", "gray", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y007_Annotations.png", plot = Img28,
       units = "px", height = max(Mask28$Height), width = max(Mask28$Width))
write.table(format_mask(Mask28), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y007_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 29------------------------------------------------------------------------
Mask29 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y008_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Purple", "Green", "Blue", "Yellow")))
Img29 <- make_plot(Mask29, c("white", "blue", "green", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y008_Annotations.png", plot = Img29,
       units = "px", height = max(Mask29$Height), width = max(Mask29$Width))
write.table(format_mask(Mask29), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y008_Annotations.txt",
            quote = F, row.names = F, sep = "\t")

# Tile 30------------------------------------------------------------------------
Mask30 <- SVG_to_CSV("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_SVG/KPMP_uS-X003Y009_Annotations.svg", 
                     Features %>% filter(symbol %in% c("Purple", "Green", "Yellow")))
Img30 <- make_plot(Mask30, c("white", "green", "purple", "yellow"))
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X003Y009_Annotations.png", plot = Img30,
       units = "px", height = max(Mask30$Height), width = max(Mask30$Width))
write.table(format_mask(Mask30), "~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X003Y009_Annotations.txt",
            quote = F, row.names = F, sep = "\t")










