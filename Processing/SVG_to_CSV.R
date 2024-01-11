library(magick)
library(tidyverse)

svg <- image_read("~/Downloads/NewTest.svg")
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

Features <- data.frame(
  symbol = c("Islet", "Islet", "Smear", "Smear"),
  red = c(0, 180, 6, 128),
  green = c(128, 217, 42, 164),
  blue = c(0, 180, 155, 175),
  thresh = c(30, 30, 30, 30)
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
  dplyr::select(-c(R, G, B, RGB))

ggplot(ProcessedImage, aes(x = Length, y = Width, fill = Cluster)) +
  geom_tile() +
  theme_bw()


