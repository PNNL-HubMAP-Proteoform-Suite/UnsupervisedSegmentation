library(data.table)
library(tidyverse)

render_cluster <- function(data, colors, order) {
  
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
    theme(legend.position = "none")
  
}

## Clusters need to be matched manually. Matches are tracked in the Metadata csv

#########################
## DIMENSION REDUCTION ##
#########################

DimMeta <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv")
subtile <- 2
root <- unique(DimMet$Path)[subtile]

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

#lapply(target, function(subtile) {
#  
#  root <- unique(Image_Metadata$Path)[subtile]
#  data <- fread(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_TXT/", 
#                          gsub(pattern = "Annotations", replacement = "KCC.txt", root)))
#  plot <- render_cluster(data,
#                         unlist(Image_Metadata[Image_Metadata$Path == root, Color]),
#                         unlist(Image_Metadata[Image_Metadata$Path == root, KCC]))
#  
#  ggsave(file.path("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", 
#                   gsub("_Annotations", "_KCC.png", root)),  plot = plot,
#         units = "px", height = nrow(data), width = ncol(data))
#})

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

##################
## PLOT FIGURES ##
##################

library(patchwork)
library(cowplot)
library(magick)

## Tiles ##

draw_fun <- function(x, y, height = 0.95, size = 16) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = height, size = size)}

Ori <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/KPMP_uS-X002Y010.png", "Original Image", 0.98, 12)
Target <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/KPMP_uS-X002Y010_Annotations.png", "Target")
Recolorize <-  draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/KPMP_uS-X002Y010_recolorize.png", "Recolorize")
KCC_Blur <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/KPMP_uS-X002Y010_KCC.png", "KCC with Blur")
KMeans <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG/KPMP_uS-X002Y010_KMeans.png", "KMeans")
Supercells <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/KPMP_uS-X002Y010_supercells.png", "Supercells")
PyImSeg <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/KPMP_uS-X002Y010.png", "pyImSegm")
Clara <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/KPMP_uS-X002Y010_Clara.png", "Clara")
PyTorch <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/KPMP_uS-X002Y010.png", "pytorch-tip")

F2 <- Ori + Target + KCC_Blur + Recolorize + KMeans + Supercells + PyImSeg + Clara + PyTorch + plot_annotation(tag_levels = "A")

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


draw_fun <- function(x, y, height = 0.95, size = 16) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = height, size = size)}
Ori <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KPMP.png", "Original Image", height = 0.98)
Clara <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Clara.png", "Clara")
KCC_Blur <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KCC_Blur.png", "KCC with Blur")
KCC <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KCC.png", "KCC")
KMeans <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/KMeans.png", "KMeans")
PyImSeg <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/PyImSeg.png", "PyImSeg")
PyTorch <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/PyTorch.png", "PyTorch")
Recolorize <-  draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Recolorize.png", "Recolorize")
Supercells <- draw_fun("~/Git_Repos/UnsupervisedSegmentation/Images/KPMP/Supercells.png", "Supercells")

F4 <- Ori + Clara + KCC_Blur + KCC + KMeans + PyImSeg + PyTorch + Recolorize + Supercells + plot_annotation(tag_levels = "A")
F4


