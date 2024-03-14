library(tidyverse)
library(data.table)
library(trelliscope)
library(cowplot)
library(magick)

########################
## KIDNEY - HISTOLOGY ##
########################

summaryTable_kidney_hist <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  select(-Notes) %>%
  rename(Image = Path) %>%
  mutate(Image = gsub(".svg", "", Image, fixed = T))

pull_unlabeled_kidney_hist <- function(Image) {
  path <- paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Original/", 
                 gsub("_Annotations", "", Image, fixed = T), ".png")
  ggdraw() + draw_image(path)
}

pull_truth_kidney_hist <- function(Image) {
  path <- paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/Truth/", Image, "_extraction.png")
  ggdraw() + draw_image(path)
}

pull_first_kidney_hist <- function(Image) {
  path <- paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney/KCC_Blur_Angle/Image/",
                 gsub("_Annotations", "_blur10_KCC_angle.png", Image, fixed = T))
  ggdraw() + draw_image(path)
}

toBuild_kidney_hist <- summaryTable_kidney_hist %>%
  mutate(
    unlabeled_image = panel_lazy(pull_unlabeled_kidney_hist),
    truth_mask = panel_lazy(pull_truth_kidney_hist),
    kcc_blur_angle = panel_lazy(pull_first_kidney_hist)
  )

as_trelliscope_df(df = toBuild_kidney_hist, name = "Kidney Histology", 
                  path = "~/Git_Repos/UnsupervisedSegmentation/Visualization/Kidney_Histology") %>%
  view_trelliscope()



