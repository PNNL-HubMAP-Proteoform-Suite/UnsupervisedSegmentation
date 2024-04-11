library(doParallel)
library(foreach)





# Process blur images 
blur_df <- data.frame(PostBlur = list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Blur", full.names = T)) %>%
  mutate(
    Mask = gsub("Blur/", "KCC_Blur_Angle/Mask/", PostBlur) %>% gsub(pattern = ".png", replacement = "_KCC_angle.txt", ., fixed = T),
    ResultImage = gsub("Blur/", "KCC_Blur_Angle/Image/", PostBlur) %>% gsub(pattern = ".png", replacement = "_KCC_angle.png", ., fixed = T),
    Clusters = Metadata$`Total Feature Classes`
  )


cl <- makeCluster(8)
registerDoParallel(cl)

foreach(x = 17:nrow(blur_df)) %dopar% { 
  
  kcc(
    in_path = blur_df$PostBlur[x],
    out_path_data = blur_df$Mask[x],
    out_path_image = blur_df$ResultImage[x],
    k = blur_df$Clusters[x],
    family = kccaFamily("angle")
  )
  
}

stopCluster(cl)

