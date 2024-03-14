library(magick)

blur <- function(in_path, out_path, radius = 100, sigma = 5) {
  img <- image_read(in_path)
  blurred <- image_blur(img, radius = radius, sigma = sigma)
  image_write(blurred, out_path)
}

# Blur all the images 
library(tidyverse)

blur_df <- data.frame(PreBlur = list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Original", full.names = T)) %>%
  mutate(PostBlur = gsub("Original", "Blur", PreBlur) %>% gsub(pattern = ".png", replacement = "_blur10.png", ., fixed = T))

run <- lapply(1:nrow(blur_df), function(x) {
  blur(
    in_path = blur_df$PreBlur[x],
    out_path = blur_df$PostBlur[x],
    sigma = 10
  )
})


