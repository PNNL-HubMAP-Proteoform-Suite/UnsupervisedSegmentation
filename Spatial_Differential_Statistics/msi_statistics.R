# Data management packages 
library(data.table)
library(tidyverse)

# Set a working directory
setwd("~/Git_Repos/UnsupervisedSegmentation/Spatial_Differential_Statistics/")

## Load MSI Data----------------------------------------------------------------

# Read in MSI data
msi <- readRDS("msi.RDS")
edata <- msi[[1]] %>%
  pivot_longer(2:ncol(.))
fdata <- msi[[2]]
rm(msi)

## MSI Data Preprocessing-------------------------------------------------------

# Log2 transform data
edata$value <- log2(edata$value)

