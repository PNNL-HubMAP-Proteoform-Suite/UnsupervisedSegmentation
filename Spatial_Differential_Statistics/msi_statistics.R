# Fit a spatial anova
library(lme4)
library(lmerTest)

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
  pivot_longer(2:ncol(.)) %>%
  na.omit()
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
  dplyr::select(X, Y, Hexadecimal) %>%
  ggplot(aes(x = X, y = Y, fill = Hexadecimal)) +
    geom_raster() +
    ylab("-1 * Y") +
    theme_void() + 
    ggtitle("Original") +
    theme(plot.title = element_text(size = 20, hjust = 0.5))

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
binning_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Binning)) %>% cluster_visualize(title = "binning")
clara_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Clara)) %>% cluster_visualize(title = "clara")
kcc_plot <- ggplot(fdata, aes(x = X, y = Y, fill = KCC)) %>% cluster_visualize(title = "KCC")
kmeans_plot <- ggplot(fdata, aes(x = X, y = Y, fill = KMeans)) %>% cluster_visualize(title = "k-means")
multiotsu_plot <- ggplot(fdata, aes(x = X, y = Y, fill = MultiOtsu)) %>% cluster_visualize(title = "Multi-Otsu")
pytorch_plot <- ggplot(fdata, aes(x = X, y = Y, fill = PyTorch)) %>% cluster_visualize(title = "pytorch-tip")
recolorize_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Recolorize)) %>% cluster_visualize(title = "recolorize")
supercells_plot <- ggplot(fdata, aes(x = X, y = Y, fill = Supercells)) %>% cluster_visualize(title = "supercells")

(feature_img + binning_plot + clara_plot) /
  (kcc_plot + kmeans_plot + multiotsu_plot) /
  (pytorch_plot + recolorize_plot + supercells_plot) + 
  plot_annotation(tag_levels = "A")

## MSI Data Pre-processing------------------------------------------------------

# Log2 transform data
edata$value <- log2(edata$value)

# Normalization (median centering)
edata <- edata %>%
  group_by(Biomolecule) %>%
  mutate(
    Median = median(value),
    value = value - Median
  ) %>%
  dplyr::select(-Median) %>%
  rename(Pixel = name, Abundance = value)

## Add grouping information-----------------------------------------------------

edata <- edata %>% 
  left_join(fdata %>% dplyr::select(1, 5:15), by = "Pixel") %>%
  mutate(Y = abs(Y)) %>%
  select(-Pixel) 

## Run Spatial ANOVAs-----------------------------------------------------------

calc_pvalue_truth <- function(x) {
  pval <- tryCatch({
    aov_model <- lmer(Abundance ~ Class + (1|X + Y), data = x)
    pval <- summary(aov_model)$coefficients[2,"Pr(>|t|)"]
  }, error = function(e) {
    return(NA)
  }
  )
  message(pval)
  return(pval)
}

edata %>%
  pivot_longer(5:ncol(.)) %>%
  rename(Algorithm = name, Class = value) %>%
  group_by(Biomolecule, Algorithm) %>%
  nest() %>%
  mutate(PVal = map_dbl(data, calc_pvalue_truth)) %>%
  select(-data) %>%
  fwrite("pvalues.csv", quote = F, row.names = F)



