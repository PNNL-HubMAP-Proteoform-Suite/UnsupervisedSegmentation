#########################
## TRYING AN ALGORITHM ##
#########################

# Data maintenance packages 
library(tidyverse)
library(png)
library(spatstat)

testImg <- readPNG("~/Downloads/Kidney_backup/KPMP_uS-X003Y006.png")

# Format data-------------------------------------------------------------------

# Convert pixels to a data.frame
convert_df <- function(the_mat, the_name) {
  the_mat %>% 
    data.frame() %>% 
    `colnames<-`(paste0("X", 1:ncol(the_mat))) %>%
    pivot_longer(1:ncol(the_mat)) %>%
    rename(X = name) %>%
    mutate(Y = rep(paste0("Y", nrow(the_mat):1), each = ncol(the_mat))) %>%
    relocate(X) %>%
    rename(!!the_name := value)
}

# Take red, green, and blue channels, and calculate the average pixel value
Img_DF <- Reduce(left_join, list(convert_df(blur(im(testImg[,,1]), sigma = 8)$v, "Red"),
                                 convert_df(blur(im(testImg[,,2]), sigma = 8)$v, "Green"),
                                 convert_df(blur(im(testImg[,,3]), sigma = 8)$v, "Blue"))) 
Img_DF$X <- gsub("X", "", Img_DF$X) %>% as.numeric()
Img_DF$Y <- gsub("Y", "", Img_DF$Y) %>% as.numeric()

# K-Means-----------------------------------------------------------------------

KMeans <- Img_DF %>% mutate(Cluster = as.factor(kmeans(Img_DF[,c("Red", "Green", "Blue")], centers = 3)$cluster))
ggplot(KMeans, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()

# Make sure labels are correct
Modeled <- KMeans %>%
  mutate(Cluster = ifelse(Cluster == 1, "Islet", ifelse(Cluster == 2, "Background", "Blood.Vessel"))) %>%
  select(X, Y, Cluster)

ModelPlot <- ggplot(Modeled, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()
ModelPlot

################
## LOAD TRUTH ##
################

library(data.table)
library(patchwork)

Truth <- fread("~/Downloads/theCSVs/KPMP_uS-X003Y006_Annotations.csv")

# Add the X and Y vector to the pivot_longer data.frame
Truth <- Truth %>%
  pivot_longer(1:1920) %>%
  rename(X = name, Cluster = value) %>%
  mutate(
    X = gsub("V", "", X) %>% as.numeric(),
    Y = rep(1080:1, each = 1920),
    Cluster = ifelse(Cluster == "", "Background", Cluster)
  )

TruthPlot <- ggplot(Truth, aes(x = X, y = Y, fill = Cluster)) + geom_tile() + theme_bw()
ModelPlot + TruthPlot # The clusters should be the same 

########################
## CALCULATE F1 SCORE ##
########################

Truth <- Truth %>% arrange(X, Y) %>% select(X, Y, Cluster)
Modeled <- Modeled %>% arrange(X, Y)

# Treat each class as a binary layer, with the exception of background
ToScore <- Truth %>%
  mutate(
    Modeled = Modeled$Cluster,
    Blood.Vessel = (Cluster == "Blood.Vessel" & Modeled == "Blood.Vessel") | 
                   (Cluster != "Blood.Vessel" & Modeled != "Blood.Vessel"),
    Islet = (Cluster == "Islet" & Modeled == "Islet") | 
            (Cluster != "Islet" & Modeled != "Islet"),
    Total = Blood.Vessel & Islet,
    Classification = ifelse(Total == TRUE & Cluster != "Background", "True Positive",
           ifelse(Total == TRUE & Cluster == "Background", "True Negative",
           ifelse(Total == FALSE & Modeled == "Background", "False Negative", "False Positive")))
  )

# Double check cases 
ToScore[ToScore$Classification == "True Positive",] %>% head(1)
ToScore[ToScore$Classification == "True Negative",] %>% head(1)
ToScore[ToScore$Classification == "False Positive",] %>% head(1)
ToScore[ToScore$Classification == "False Negative",] %>% head(1)

# Calculate an F1 score
ToScore %>%
  group_by(Classification) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Classification, values_from = Count) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(
    Precision = `True Positive` / (`True Positive` + `False Positive`),
    Recall = `True Positive` / (`True Positive` + `False Negative`), 
    F1 = (2 * Precision * Recall) / (Precision + Recall),
    F1 = ifelse(is.nan(F1), 0, F1),
    Accuracy = (`True Positive` + `True Negative`) / nrow(ToScore),
    TPR = Recall, 
    TNR = `True Negative` / (`True Negative` + `False Positive`)
  )
  
  


