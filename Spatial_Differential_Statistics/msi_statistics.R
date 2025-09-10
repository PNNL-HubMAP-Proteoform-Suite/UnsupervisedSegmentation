# Load clustering packages
library(cluster)
library(flexclust)
library(recolorize)

# Data management packages 
library(data.table)
library(tidyverse)

# Data visualization packages
library(viridis)
library(patchwork)

# Set a working directory
setwd("~/Git_Repos/UnsupervisedSegmentation/Spatial_Differential_Statistics/")

## Load MSI Data----------------------------------------------------------------

# Read in MSI data
msi <- readRDS("msi.RDS")
edata <- msi[[1]] %>%
  pivot_longer(2:ncol(.)) 
fdata <- msi[[2]]
rm(msi)

## Ensure MSI and Feature Image are the Correct Orientations--------------------

# Visualize feature image
feature_img <- fdata %>%
  mutate(
    Hexadecimal = lapply(1:nrow(.), function(row) {
      grDevices::rgb(.$R[row]/255, .$G[row]/255, .$B[row]/255)
    })
  ) %>%
  select(X, Y, Hexadecimal) %>%
  ggplot(aes(x = X, y = Y, fill = Hexadecimal)) +
    geom_raster() +
    ylab("-1 * Y") +
    theme_void()
feature_img

# Visualize an example MSI image
example_msi_image <- edata %>%
  filter(Biomolecule == "CH2O2[M]+") %>% 
  rename(Abundance = value) %>%
  mutate(
    X = map_dbl(name, function(x) {
      strsplit(x, "Y") %>% unlist() %>% head(1) %>% gsub("X", "", .) %>% as.numeric()
    }),
    Y = map_dbl(name, function(x) {
      strsplit(x, "Y") %>% unlist() %>% tail(1) %>% as.numeric()
    })
  ) %>%
  mutate(Y = -1 * Y) %>%
  ggplot(aes(x = X, y = Y, fill = Abundance)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis(na.value = "black") +
    theme_classic() +
    theme(legend.position = "right", plot.title = element_text(hjust = 0.5))  
example_msi_image


## MSI Data Pre-processing------------------------------------------------------

# Log2 transform data
edata$value <- log2(edata$value)

# Normalization (median centering)
edata <- edata %>%
  group_by(Biomolecule) %>%
  mutate(
    Median = median(value),
    value = value - Median
  )

## Add Segmentation Group Information-------------------------------------------

# Visualize a cluster
cluster_visualize <- function(x, title) {
  x + 
    geom_raster(interpolate = TRUE) + 
    scale_fill_manual(values = c("white", "black")) + 
    ggtitle(title) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 20, hjust = 0.5))
}

# fdata add annotations --> just once! 
#fdata <- left_join(fdata, fread("Feature_annotated.txt") %>% 
#                     rename(X = Width, Y = Height, Truth = Cluster) %>%
#                     mutate(X = X - 1,
#                            Y = -1 * (216-Y),
#                            Truth = ifelse(Truth == 1, "Not Root", "Root")))
manual_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Truth)) %>% cluster_visualize(title = "Manual Segmentation")

# For each method, cluster once and save results.

## CLARA ## 
#fdata <- fdata %>% mutate(Clara = clara(.[,c("R", "G", "B")], 2)$clustering)
#fdata <- fdata %>% mutate(Clara = ifelse(Clara == 1, "Not Root", "Root"))
clara_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Clara)) %>% cluster_visualize(title = "clara")

## KCC ## 
#fdata <- fdata %>% mutate(KCC = kcca(.[,c("R", "G", "B")], 2)@cluster)
#fdata <- fdata %>% mutate(KCC = ifelse(KCC == 1, "Not Root", "Root")) 
kcc_plot <- ggplot(fdata, aes(x = X, y = Y, fill = KCC)) %>% cluster_visualize(title = "KCC")

## KMeans ## 
#fdata <- fdata %>% mutate(KMeans = kmeans(.[,c("R", "G", "B")], 2)$cluster)
#fdata <- fdata %>% mutate(KMeans = ifelse(KMeans == 1, "Root", "Not Root")) 
kmeans_plot <- ggplot(fdata, aes(x = X, y = Y, fill = KMeans)) %>% cluster_visualize(title = "k-means")

## Recolorize ##
# source("../Algorithms/recolorize.R")
#apply_recolorize("Feature.png", 2, ".", blur = FALSE)
#fdata <- fdata %>% 
#  mutate(Recolorize = fread("Feature_recolorize.txt") %>%
#           mutate(Height = 1:nrow(.)) %>%
#           pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
#           arrange(-Height) %>%
#           select(value) %>% 
#           unlist()
#  ) %>%
#  mutate(Recolorize = ifelse(Recolorize == 1, "Root", "Not Root")) 
recolorize_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Recolorize)) %>% 
  cluster_visualize(title = "recolorize")

## Supercells ##
#source("../Algorithms/supercells_root.R")
#apply_supercells("Feature.png", 2, ".", blur = FALSE)
#fdata <- fdata %>% 
#  mutate(Supercells = fread("Feature_supercells.txt") %>%
#             mutate(Height = 1:nrow(.)) %>%
#             pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
#             arrange(-Height) %>%
#             select(value) %>% 
#             unlist()
#  ) %>%
#  mutate(Supercells = ifelse(Supercells == 1, "Root", "Not Root"))  
supercells_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Supercells)) %>% cluster_visualize(title = "supercells")

## Binning ## 
fdata <- fdata %>% 
  mutate(Binning = fread("Feature_binning.txt", header = T) %>%
             mutate(Height = 1:nrow(.)) %>%
             pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
             arrange(-Height) %>%
             select(value) %>% 
             unlist()
  ) %>%
  mutate(Binning = ifelse(Binning == 1, "Not Root", "Root")) 
binning_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Binning)) %>% cluster_visualize(title = "binning")

## Multi-Otsu ##
fdata <- fdata %>% 
  mutate(MultiOtsu = fread("Feature_multiotsu.txt", header = T) %>%
           mutate(Height = 1:nrow(.)) %>%
           pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
           arrange(-Height) %>%
           select(value) %>% 
           unlist()
  ) %>%
  mutate(MultiOtsu = ifelse(MultiOtsu == 1, "Not Root", "Root"))
multiotsu_plot <- ggplot(fdata, aes(x = X, y = Y, fill = MultiOtsu)) %>% cluster_visualize(title = "Multi-Otsu")

## PyTorchTip ##
fdata <- fdata %>% 
  mutate(PyTorch = fread("Feature_PyTorch.txt", header = T) %>%
           mutate(Height = 1:nrow(.)) %>%
           pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
           arrange(-Height) %>%
           select(value) %>% 
           unlist()
  ) %>%
  mutate(PyTorch = ifelse(PyTorch == 6, "Not Root", "Root"))
pytorch_plot <- ggplot(fdata, aes(x = X, y = Y, fill = PyTorch)) %>% cluster_visualize(title = "pytorch-tip")





