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
    select(-c(R,G,B,RGB))
  
  thePlot <- ggplot(ProcessedImage, aes(x = Length, y = Width, fill = Cluster)) +
    geom_tile() +
    theme_bw()
  
  
  return(list(ProcessedImage, thePlot))
  
}

## Make List of Features ## 
Features <- data.frame(
  symbol = c("Islet", "Islet", "Smear", "Smear", "Glomerulus", "Glomerulus",
             "Tubule", "Tubule", "Blood.Vessel", "Blood.Vessel", "Empty.Space", "Empty.Space"),
  red = c(0, 195, 234, 249, 183, 244, 43, 191, 70, 199, 192, 230),
  green = c(126, 216, 51, 193, 171, 230, 41, 190, 8, 180, 190, 235),
  blue = c(0, 188, 35, 189, 193, 245, 120, 214, 27, 186, 62, 197),
  thresh = c(30, 30, 30, 30, 20, 20, 30, 30, 30, 30, 20, 20)
)

## Pull SVGs ##
SVGs <- list.files("/Users/degn400/Library/Group Containers/UBF8T346G9.OneDriveStandaloneSuite/OneDrive - PNNL.noindex/OneDrive - PNNL/Desktop/HubMAP/ImageIntegrator/Annotations_Final/Kidney",
                  full.names = T)
SVGs <- SVGs[grepl("svg", SVGs)]

## Read Annotations ## 
Annotations <- read.csv("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

# Iterate through each image, and make sure there is the correct number of clusters 
lapply(SVGs, function(SVG) {
  
  # Simplify the path
  SimplePath <- SVG %>% strsplit("/") %>% unlist() %>% tail(1)
  message(SimplePath)
  
  # Pull known features
  Known <- Annotations[Annotations$Path == SimplePath, 2:7]
  KnownFeatures <- colnames(Known)[Known != 0]
  
  # Run pipeline
  Res <- SVG_to_CSV(SVG, Features %>% filter(symbol %in% KnownFeatures))
  
  # Make sure the number of clusters is correct
  if (length(unique(Res[[1]]$Cluster)) == Annotations[Annotations$Path == SimplePath, "Total.Feature.Classes"]) {
    NewPath <- gsub(".svg", ".csv", SimplePath, fixed = T)
    NewHeader <- SVGs[1] %>% strsplit("/") %>% unlist() %>% head(-1) %>% paste0(collapse = "/")
    NewPath <- file.path(NewHeader, NewPath)
    CleanResults <- matrix(Res[[1]]$Cluster, nrow = max(Res[[1]]$Width), ncol = max(Res[[1]]$Length))
    CleanResults[CleanResults == "Background"] <- NA
    fwrite(CleanResults, NewPath)
    ggsave(filename = gsub(".csv", "_extraction.png", NewPath), plot = Res[[2]])
  } else {
    browser()
  }
  
})









