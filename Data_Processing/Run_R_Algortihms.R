library(tidyverse)
library(doParallel)
library(foreach)

# Start an image data.frame
images <- data.frame(
  Original = rep(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/NewCohort/Original_Image", full.names = T), 3),
  K = rep(3:5, each = 9)
) 

##################
## KCC + Blur10 ##
##################

# Source function
source("~/Git_Repos/UnsupervisedSegmentation/Algorithms/kcc_blur10.R")

# Add outputs 
images <- images %>%
  mutate(
    KCC_Out = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Original_Image", replacement = "KCC_Blur10", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCCBlur10_", y, ".png"))
    }), 
    KCC_Out_Data = map2_chr(Original, K, function(x, y) {
      gsub(pattern = "Original_Image", replacement = "KCC_Blur10", x = x) %>%
        gsub(pattern = ".png|.jpg|.tif", replacement = paste0("_KCCBlur10_", y, ".txt"))
    })
  )

cl <- makeCluster(8); registerDoParallel(cl)
foreach(x = 1:nrow(images)) %dopar% { 
  kcc_blur10(
    in_path = images$Original[x],
    out_path_data = images$KCC_Out_Data[x],
    out_path_image = images$KCC_Out[x],
    k = images$K[x]
  )
}
stopCluster(cl)

