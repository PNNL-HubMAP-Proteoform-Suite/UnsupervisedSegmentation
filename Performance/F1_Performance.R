library(tidyverse)
library(data.table)

#####################
## BLUR COMPARISON ##
#####################

# Load and format data
KM_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KMeans_Blur_Counts.csv")
KM <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KMeans_Counts.csv") %>% 
  filter(Image %in% KM_Blur$Image)
KCC <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KCC_Counts.csv")
KCC_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KCC_Blur_Counts.csv")
Clara <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Clara_Counts.csv")
Clara_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Clara_Blur_Counts.csv")
Scell <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Supercell_Counts.csv")
Scell_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Supercell_Blur_Counts.csv")
Re <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Recolorize_Counts.csv")
Re_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/Recolorize_Blur_Counts.csv")
PT <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/PyTorch_Counts.csv")
PT_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/PyTorch_Blur_Counts.csv")
PY <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/pyImSeg_Counts.csv")
PY_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/pyImSeg_Blur_Counts.csv")

# Calculate balanced accuracy 
BA <- rbind(
  KM %>% mutate(Algorithm = "K-Means", Format = "Original"),
  KM_Blur %>% mutate(Algorithm = "K-Means", Format = "Blur"),
  KCC %>% mutate(Algorithm = "KCC", Format = "Original"),
  KCC_Blur %>% mutate(Algorithm = "KCC", Format = "Blur"),
  Clara %>% mutate(Algorithm = "Clara", Format = "Original"),
  Clara_Blur %>% mutate(Algorithm = "Clara", Format = "Blur"),
  Scell %>% mutate(Algorithm = "Supercells", Format = "Original"),
  Scell_Blur %>% mutate(Algorithm = "Supercells", Format = "Blur"),
  Re %>% mutate(Algorithm = "Recolorize", Format = "Original"),
  Re_Blur %>% mutate(Algorithm = "Recolorize", Format = "Blur"),
  PT %>% mutate(Algorithm = "PyTorch-Tip", Format = "Original"),
  PT_Blur %>% mutate(Algorithm = "PyTorch-Tip", Format = "Blur"),
  PY %>% mutate(Algorithm = "PyImSeg", Format = "Original"),
  PY_Blur %>% mutate(Algorithm = "PyImSeg", Format = "Blur")
) %>%
  pivot_wider(id_cols = c(Cluster, Image, Algorithm, Format), names_from = Counts, values_from = Freq) %>%
  mutate(
    `True Positive` = ifelse(is.na(`True Positive`), 0, `True Positive`),
    Precision = `True Positive` / (`True Positive` + `False Positive`),
    Recall = `True Positive` / (`True Positive` + `False Negative`), 
    F1 = (2 * Precision * Recall) / (Precision + Recall),
    BA = ((`True Positive` / (`True Positive` + `False Negative`)) + 
         (`True Negative` / (`True Negative` + `False Positive`))) / 2,
  ) 
  

# Make plots 
BA %>% select(Cluster, Algorithm, Format, BA) %>%
  mutate(Format = factor(Format, levels = c("Original", "Blur"))) %>%
  ggplot(aes(x = Algorithm, y = BA, fill = Format)) +
    geom_boxplot() + 
    theme_bw() +
    ylim(c(0,1)) + 
    ylab("Balanced Accuracy") 

# Calculate paired t-tests
BA %>%
  select(Cluster, Image, Algorithm, Format, BA) %>%
  group_by(Algorithm) %>%
  nest() %>%
  arrange(Algorithm) %>%
  mutate(
    TTest = map_dbl(data, function(x) {
      pairs <- x %>% 
        pivot_wider(names_from = Format, values_from = BA, id_cols = c(Cluster, Image))
      t.test(x = pairs$Original, y = pairs$Blur, alternative = "less", paired = TRUE)$p.value
    })
  ) %>%
  select(Algorithm, TTest)


###################
## FULL ANALYSIS ##
###################

data <- do.call(rbind, lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/", full.names = T), function(file) {
  data <- fread(file)
  data$Method <- strsplit(file, "/", fixed = T) %>% unlist() %>% tail(1) %>% gsub(pattern = "_Counts.csv", replacement = "")
  return(data)
}))
  
Stats_Table <- data %>% 
  pivot_wider(id_cols = c(Cluster, Image, Method), names_from = Counts, values_from = Freq) %>%
  mutate(
    Precision = `True Positive` / (`True Positive` + `False Positive`),
    Recall = `True Positive` / (`True Positive` + `False Negative`), 
    F1 = (2 * Precision * Recall) / (Precision + Recall),
    F1 = ifelse(is.nan(F1), 0, F1),
    TPR = Recall, 
    TNR = `True Negative` / (`True Negative` + `False Positive`),
    Precision = round(Precision, 4),
    Recall = round(Recall, 4),
    F1 = round(F1, 4),
    TPR = round(TPR, 4),
    TNR = round(TNR, 4)
  )
  
# Make plots--------------------------------------------------------------------

PerformancePlot <- rbind(
  left_join(
    Stats_Table %>% filter(Cluster == 1) %>% select(Image, Method, F1),
    Stats_Table %>% group_by(Image, Method) %>% summarize(`Number of Clusters` = n())
  ) %>% mutate(Type = "Background Cluster"),
  left_join(
    Stats_Table %>% filter(Cluster != 1) %>% select(Image, Method, F1) %>% group_by(Image, Method) %>% summarise(F1 = mean(F1, na.rm = T)),
    Stats_Table %>% group_by(Image) %>% summarize(`Number of Clusters` = n())
  ) %>% mutate(Type = "Feature Clusters (Mean)")
) %>%
  mutate(`Number of Clusters` = as.factor(`Number of Clusters`)) %>%
  ggplot(aes(x = `Number of Clusters`, y = F1, fill = `Number of Clusters`)) +
    geom_boxplot() + 
    geom_jitter(width = 0.1, height = 0.05) + 
    theme_bw() +
    ylim(c(0, 1)) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    facet_grid(rows = vars(Method), cols = vars(Type))

PerformancePlot




