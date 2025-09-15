library(data.table)
library(tidyverse)

render_cluster <- function(data, colors, order, title) {
  
  colorList <- colors
  names(colorList) <- order
  
  data %>%
    mutate(Height = 1:nrow(.)) %>%
    pivot_longer(cols = c(1:(ncol(.) - 1))) %>%
    rename(Cluster = value, Width = name) %>%
    mutate(Width = gsub("V", "", Width) %>% as.numeric(),
           Cluster = as.factor(Cluster)) %>% 
    ggplot(aes(x = Width, y = Height, fill = Cluster)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_manual(values = colorList) +
      theme_void() +
      ggtitle(title) + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 
  
}

## Clusters need to be matched manually. Matches are tracked in the Metadata csv

#########################
## DIMENSION REDUCTION ##
#########################

DimMeta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv")
subtile <- 9
root <- unique(DimMeta$Path)[subtile]

PCA <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PCA_KCC_TXT", gsub(pattern = "Annotations", replacement = "PCA_KCC.txt", root)))
PCAplot <- render_cluster(PCA, unlist(DimMeta[DimMeta$Path == root, Color]), unlist(DimMeta[DimMeta$Path == root, KCC.PCA]))
PCAplot 
ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PCA_KCC_PNG", gsub("_Annotations", "_PCA_KCC.png", root)), plot = PCAplot, units = "px", height = nrow(PCA), width = ncol(PCA))


DimMeta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv")
tSNE <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/tSNE_KCC_TXT/", gsub(pattern = "Annotations", replacement = "tSNE_KCC.txt", root)))
tSNEplot <- render_cluster(tSNE, unlist(DimMeta[DimMeta$Path == root, Color]), unlist(DimMeta[DimMeta$Path == root, KCC.tSNE]))
tSNEplot 
ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/tSNE_KCC_PNG", gsub("_Annotations", "_tSNE_KCC.png", root)), plot = tSNEplot, units = "px", height = nrow(tSNE), width = ncol(tSNE))


DimMeta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv")
SVD <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/SVD_KCC_TXT", gsub(pattern = "Annotations", replacement = "SVD_KCC.txt", root)))
SVDplot <- render_cluster(SVD, unlist(DimMeta[DimMeta$Path == root, Color]), unlist(DimMeta[DimMeta$Path == root, KCC.SVD]))
SVDplot 
ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/SVD_KCC_PNG", gsub("_Annotations", "_SVD_KCC.png", root)), plot = SVDplot, units = "px", height = nrow(SVD), width = ncol(SVD))

#####################
## BLUR/FULL STUDY ##
#####################

# KMeans------------------------------------------------------------------------

target <- 18:20

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT", 
                          gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Kmeans]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG", 
                   gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})

#lapply(target, function(subtile) {
#  
#  root <- unique(Image_Metadata$Path)[subtile]
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_TXT/", 
#                          gsub(pattern = "Annotations", replacement = "KMeans.txt", root)))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Kmeans.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_PNG", 
#                   gsub("_Annotations", "_KMeans.png", root)),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#  
#})


# KCC---------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

new_targets <- Image_Metadata$Path %>% unique()

lapply(new_targets[20], function(root) {
  
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT/", 
                          gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, KCC]))
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", 
                   gsub("_Annotations", "_KCC.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

lapply(target, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_TXT/", 
                          gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, KCC.Blur]))
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", 
                   gsub("_Annotations", "_KCC.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})


# Clara-------------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {

  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_TXT/", 
                          gsub(pattern = "Annotations", replacement = "Clara.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Clara]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/", 
                   gsub("_Annotations", "_Clara.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})

#lapply(target, function(subtile) {
#  
#  root <- unique(Image_Metadata$Path)[subtile]
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_Blur_TXT/", 
#                          gsub(pattern = "Annotations", replacement = "Clara.txt", root)))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Clara.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_Blur_PNG/", 
#                   gsub("_Annotations", "_Clara.png", root)),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#  
#})



# Supercells---------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_TXT/", 
                          gsub(pattern = "Annotations", replacement = "supercells.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Supercells]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", 
                   gsub("_Annotations", "_supercells.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})


#lapply(target, function(subtile) {
#  root <- unique(Image_Metadata$Path)[subtile]
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_TXT/", 
#                          gsub(pattern = "Annotations", replacement = "supercells.txt", root)))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Supercells.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_PNG/", 
#                   gsub("_Annotations", "_supercells.png", root)),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#})

# Recolorize--------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_TXT/", 
                          gsub(pattern = "Annotations", replacement = "recolorize.txt", root)))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, Recolorize]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", 
                   gsub("_Annotations", "_recolorize.png", root)),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})

#lapply(target, function(subtile) {
#
#  root <- unique(Image_Metadata$Path)[subtile]
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_TXT/", 
#                          gsub(pattern = "Annotations", replacement = "recolorize.txt", root)))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Recolorize.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_PNG/", 
#                   gsub("_Annotations", "_recolorize.png", root)),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#  
#})

# PyTorchTip--------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  newroot <- gsub("_Annotations", "", root)
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_TXT/", paste0(newroot, ".txt")))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, PyTorch]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/", paste0(newroot, ".png")), plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})


#lapply(target, function(subtile) {
#  
#  root <- unique(Image_Metadata$Path)[subtile]
#  newroot <- gsub("_Annotations", "", root)
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_Blur_TXT/",  paste0(newroot, ".txt")))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, PyTorch.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_Blur_PNG/",  paste0(newroot, ".png")),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#  
#})


# PyImSeg--------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur != "X")

lapply(target, function(subtile) {
  
  root <- unique(Image_Metadata$Path)[subtile]
  newroot <- gsub("_Annotations", "", root)
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_TXT/", paste0(newroot, ".txt")))
  plot <- render_cluster(data,
                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
                         unlist(Image_Metadata[Image_Metadata$Path == root, PyImSeg]))
  plot
  
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/", paste0(newroot, ".png")),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
  
})


#lapply(target, function(subtile) {
#  
#  root <- unique(Image_Metadata$Path)[subtile]
#  newroot <- gsub("_Annotations", "", root)
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_Blur_TXT/", paste0(newroot, ".txt")))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, PyImSeg.Blur]))
#  plot
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_Blur_PNG/", paste0(newroot, ".png")),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#  
#})

# MultiOtsu---------------------------------------------------------------------

Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")

target <- 30
lapply(target, function(subtile) {
  root <- unique(Image_Metadata$Path)[subtile]
  newroot <- gsub("_Annotations", "_multiotsu", root)
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Multiotsu_TXT/", paste0(newroot, ".txt")))
  plot <- render_cluster(
    data,
    unlist(Image_Metadata[Image_Metadata$Path == root, "Color"]),
    unlist(Image_Metadata[Image_Metadata$Path == root, "MultiOtsu"])
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Multiotsu_PNG/", paste0(newroot, ".png")),  plot = plot,
                   units = "px", height = nrow(data), width = ncol(data))
})

# Binning-----------------------------------------------------------------------
  
Image_Metadata <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv")
target <- 30
lapply(target, function(subtile) {
  root <- unique(Image_Metadata$Path)[subtile]
  newroot <- gsub("_Annotations", "_binning", root)
  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Binning_TXT/", paste0(newroot, ".txt")))
  plot <- render_cluster(
    data,
    unlist(Image_Metadata[Image_Metadata$Path == root, "Color"]),
    unlist(Image_Metadata[Image_Metadata$Path == root, "Binning"])
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Binning_PNG/", paste0(newroot, ".png")),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

##################
## PLOT FIGURES ##
##################

library(patchwork)
library(cowplot)
library(magick)

## Tiles ##

draw_fun <- function(x, y, height = 0.95, size = 16) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = height, size = size)}

# Pull the original plot
Ori <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/KPMP_uS-X002Y010.png", "Original Image", 0.98, 12)

# Set colors
colorPal <- c("white", "#0072B2", "orange")

# Make target plot and others
Target <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_TXT/KPMP_uS-X002Y010_Annotations.txt"), 
               colorPal, c(1,2,3), "Target")
Binning <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Binning_TXT/KPMP_uS-X002Y010_binning.txt"), 
               colorPal, c(2,3,1), "Binning")
Clara <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_TXT/KPMP_uS-X002Y010_clara.txt"), 
               colorPal, c(2,3,1), "Clara")
KMeans <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_TXT/KPMP_uS-X002Y010_KMeans.txt"),
               colorPal, c(3,1,2), "K-Means")
KCC <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT/KPMP_uS-X002Y010_KCC.txt"),
               colorPal, c(2,3,1), "KCC")
MO <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Multiotsu_TXT/KPMP_uS-X002Y010_multiotsu.txt"),
               colorPal, c(2,3,1), "Multi-Otsu")
ImSeg <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_TXT/KPMP_uS-X002Y010.txt"),
               colorPal, c(2,1,3), "pyImSegm")
pytorch <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_TXT/KPMP_uS-X002Y010.txt"),
               colorPal, c(3,1,2), "pytorch-tip")
Recolorize <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_TXT/KPMP_uS-X002Y010_recolorize.txt"),
               colorPal, c(1,2,3), "Recolorize")
Supercells <- render_cluster(fread("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_TXT/KPMP_uS-X002Y010_supercells.txt"),
               colorPal, c(3,1,2), "Supercells")

# Figure 2
F2 <- Target + Binning + Clara + KMeans + KCC + MO + ImSeg + pytorch + Recolorize + Supercells +
  plot_layout(nrow = 2, ncol = 5) + plot_annotation(tag_levels = "A")

F2

## Full Tissue ## 

# Go through each one at a time and match cluster numbers
Full_Meta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/KPMP.csv")
data <- fread("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Supercells.txt")

plot <- render_cluster(data,
                       unlist(Full_Meta[, Color]),
                       unlist(Full_Meta[, Supercells]))
plot
ggsave("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Supercells.png", units = "px", height = nrow(data), width = ncol(data),
       plot = plot)


#-------------------------------------------------------------------------------
library(cowplot)

draw_fun <- function(x, y, height = 0.95, size = 16) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = height, size = size)}
Ori <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", "Original Image", height = 0.98)
Clara <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Clara.png", "clara")
KCC_Blur <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KCC_Blur.png", "KCC with Blur")
KCC <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KCC.png", "KCC")
KMeans <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KMeans.png", "K-Means")
PyImSeg <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/PyImSeg.png", "pyImSegm")
PyTorch <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/PyTorch.png", "pytorch-tip")
Recolorize <-  draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Recolorize.png", "recolorize")
Supercells <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Supercells.png", "supercells")

F4 <- Ori + Clara + KCC_Blur + KCC + KMeans + PyImSeg + PyTorch + Recolorize + Supercells + plot_annotation(tag_levels = "A")
F4

## Root-------------------------------------------------------------------------

root_im <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Root.csv")

## Binning
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Binning_TXT", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_binning.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Binning"]),
    "Binning"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Binning_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## Clara
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Clara_TXT", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_CLARA.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Clara"]),
    "Clara"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Clara_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## KCC
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KCC_TXT", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_KCC.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "KCC"]),
    "KCC"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KCC_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## KMeans
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KMeans_TXT/", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_KMeans.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Kmeans"]),
    "KMeans"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KMeans_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## Multi-Otsu
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Multiotsu_TXT/", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_multiotsu.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Multiotsu"]),
    "Multi-Otsu"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Multiotsu_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## pytorch-tip
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/PyTorch_TXT/", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = ".txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Pytorch"]),
    "pytorch-tip"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/PyTorch_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## Recolorize
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Recolorize_TXT/", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_recolorize.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Recolorize"]),
    "Recolorize"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Recolorize_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## Supercells
lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Supercells_TXT/", full.names = T), function(x) {
  title <- x %>% strsplit("/") %>% unlist() %>% tail(1)
  root <- gsub(pattern = "_supercells.txt", replacement = "", title, fixed = T)
  newroot <- title %>% gsub(pattern = ".txt", replacement = ".png", fixed = T)
  data <- fread(x)
  plot <- render_cluster(
    data,
    unlist(root_im[root_im$Image == root, "Color"]),
    unlist(root_im[root_im$Image == root, "Supercells"]),
    "Supercells"
  )
  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Supercells_PNG/", newroot),  plot = plot,
         units = "px", height = nrow(data), width = ncol(data))
})

## Additional Images------------------------------------------------------------

# Leaf
setwd("~/Git_Repos/UnsupervisedSegmentation/Images/Leaf/")

meta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/AdditionalImages.csv")
(ggdraw() + draw_image("../../Data_Processing/AdditionalExamples/Leaf.png", width = 1, height = 0.9) + draw_label("Original", y = 0.95, size = 14)) +
  render_cluster(fread("Leaf_binning.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Binning"]), "binning") + 
  render_cluster(fread("Leaf_CLARA.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Clara"]), "clara") +
  render_cluster(fread("Leaf_KCC.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "KCC"]), "kcc") +
  render_cluster(fread("Leaf_KMeans.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Kmeans"]), "k-means") + 
  render_cluster(fread("Leaf_multiotsu.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "MultiOtsu"]), "Multi-Otsu") +
  render_cluster(fread("Leaf_PyTorch.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Pytorch"]), "pytorch-tip") +
  render_cluster(fread("Leaf_recolorize.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Recolorize"]), "recolorize") +
  render_cluster(fread("Leaf_supercells.txt"), unlist(meta[meta$Image == "Leaf", "Color"]), unlist(meta[meta$Image == "Leaf", "Supercells"]), "supercells") +
  plot_annotation(tag_levels = "A")

# Root
setwd("~/Git_Repos/UnsupervisedSegmentation/Images/RootCrossSection/")

meta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/AdditionalImages.csv")
(ggdraw() + draw_image("../../Data_Processing/AdditionalExamples/RootCrossSection.png", width = 1, height = 0.9) + draw_label("Original", y = 0.95, size = 14)) +
  render_cluster(fread("RootCrossSection_binning.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Binning"]), "binning") + 
  render_cluster(fread("RootCrossSection_CLARA.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Clara"]), "clara") +
  render_cluster(fread("RootCrossSection_KCC.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "KCC"]), "kcc") +
  render_cluster(fread("RootCrossSection_KMeans.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Kmeans"]), "k-means") + 
  render_cluster(fread("RootCrossSection_multiotsu.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "MultiOtsu"]), "Multi-Otsu") +
  render_cluster(fread("RootCrossSection_PyTorch.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Pytorch"]), "pytorch-tip") +
  render_cluster(fread("RootCrossSection_recolorize.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Recolorize"]), "recolorize") +
  render_cluster(fread("RootCrossSection_supercells.txt"), unlist(meta[meta$Image == "Root", "Color"]), unlist(meta[meta$Image == "Root", "Supercells"]), "supercells") +
  plot_annotation(tag_levels = "A")




