library(tidyverse)
library(data.table)
library(patchwork)
library(ggsignif)
library(ggdendro)

#########################
## DIMENSION REDUCTION ##
#########################

KCC <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/KCC_Counts.csv") %>% mutate(Method = "KCC")
PCA <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/PCA_KCC_Counts.csv") %>% mutate(Method = "PCA & KCC")
tSNE <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/tSNE_KCC_Counts.csv") %>% mutate(Method = "t-SNE & KCC")
SVD <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/SVD_KCC_Counts.csv") %>% mutate(Method = "SVD & KCC")

# Calculate balanced accuracies  
DR_Table <- rbind(KCC, PCA, tSNE, SVD) %>% 
  pivot_wider(id_cols = c(Cluster, Image, Method), names_from = Counts, values_from = Freq) %>%
  mutate(
    `True Positive` = ifelse(is.na(`True Positive`), 0, `True Positive`),
    BA = ((`True Positive` / (`True Positive` + `False Negative`)) + 
            (`True Negative` / (`True Negative` + `False Positive`))) / 2,
  )

# Check assumptions of ANOVA
Check <- DR_Table %>% 
  select(Method, BA) %>%
  filter(!is.na(BA)) %>%
  group_by(Method) %>%
  mutate(Residuals = BA - mean(BA))
plot(qqnorm(Check$Residuals))
qqline(Check$Residuals) # Normality assumption is ok
ggplot(Check, aes(x = Method, y = Residuals)) + geom_boxplot() + theme_bw() # Equal variance is ok

# Calculate an ANOVA and get the p-values for the multiple comparison adjustment 
myanova <- lm(BA~Method, data = DR_Table)
summary(myanova)
TukeyHSD(aov(BA~Method, data = DR_Table))


DR_Plot <- ggplot(DR_Table, aes(x = Method, y = BA)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(width = 0.1, height = 0, alpha = 0.25) +
  theme_bw() +
  ylim(c(0,1)) + 
  ylab("Balanced Accuracy") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))#+
  #theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 14),
  #      axis.title.y = element_text(size = 14))
DR_Plot

#####################
## BLUR COMPARISON ##
#####################

# Load and format data
KM_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KMeans_Blur_Counts.csv")
KM <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KMeans_Counts.csv") %>% 
  filter(Image %in% KM_Blur$Image)
KCC <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KCC_Counts.csv")
KCC_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/KCC_Blur_Counts.csv")
Clara <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Clara_Counts.csv")
Clara_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Clara_Blur_Counts.csv")
Scell <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Supercell_Counts.csv")
Scell_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Supercell_Blur_Counts.csv")
Re <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Recolorize_Counts.csv")
Re_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/Recolorize_Blur_Counts.csv")
PT <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyTorch_Counts.csv")
PT_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyTorch_Blur_Counts.csv")

# Calculate balanced accuracy 
BA <- rbind(
  KM %>% mutate(Algorithm = "K-Means", Format = "Original"),
  KM_Blur %>% mutate(Algorithm = "K-Means", Format = "Blur"),
  KCC %>% mutate(Algorithm = "KCC", Format = "Original"),
  KCC_Blur %>% mutate(Algorithm = "KCC", Format = "Blur"),
  Clara %>% mutate(Algorithm = "clara", Format = "Original"),
  Clara_Blur %>% mutate(Algorithm = "clara", Format = "Blur"),
  Scell %>% mutate(Algorithm = "supercells", Format = "Original"),
  Scell_Blur %>% mutate(Algorithm = "supercells", Format = "Blur"),
  Re %>% mutate(Algorithm = "recolorize", Format = "Original"),
  Re_Blur %>% mutate(Algorithm = "recolorize", Format = "Blur"),
  PT %>% mutate(Algorithm = "pytorch-tip", Format = "Original"),
  PT_Blur %>% mutate(Algorithm = "pytorch-tip", Format = "Blur")
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
BA_Plot <- BA %>% select(Cluster, Algorithm, Format, BA) %>%
  mutate(Format = factor(Format, levels = c("Original", "Blur")),
         Cluster = as.factor(Cluster)) %>%
  ggplot(aes(x = Algorithm, y = BA, fill = Format)) +
    geom_boxplot(outlier.shape = NA) +
    #geom_jitter(alpha = 0.25) +
    theme_bw() +
    ylim(c(0,1.05)) + 
    ylab("Balanced Accuracy") +
    xlab("") #+
  #theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
  #      axis.title.y = element_text(size = 14), legend.text = element_text(size = 14))

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
      t.test(x = pairs$Original, y = pairs$Blur, alternative = "two.sided", paired = TRUE)$p.value
    })
  ) %>%
  select(Algorithm, TTest)

DR_Plot + BA_Plot + plot_layout(widths = c(1,2)) + plot_annotation(tag_levels = "A")

###################
## FULL ANALYSIS ##
###################

count_files <- append(
  list.files("~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts", full.names = T),
  list.files("~/Git_Repos/UnsupervisedSegmentation/Performance/Root_Counts", full.names = T)
)

# Load all files
all_counts <- do.call(rbind, lapply(count_files, function(file) {
  data <- fread(file)
  data$Method <- strsplit(file, "/", fixed = T) %>% unlist() %>% tail(1) %>% gsub(pattern = "_Counts.csv", replacement = "")
  return(data)
})) %>%
  filter(Method != "KCC_Blur") %>%
  mutate(
    Method = tolower(Method),
    Method = ifelse(Method == "binning.csv", "binning", Method),
    Method = ifelse(Method == "kcc", "KCC", Method),
    Method = ifelse(Method == "kmeans", "k-means", Method),
    Method = ifelse(Method == "multiotsu.csv", "Multi-Otsu", Method),
    Method = ifelse(Method == "multiotsu", "Multi-Otsu", Method),
    Method = ifelse(Method == "pytorch", "pytorch-tip", Method)
  )

# Calculate balanced accuracies  
Stats_Table <- all_counts %>% 
  pivot_wider(id_cols = c(Cluster, Image, Method), names_from = Counts, values_from = Freq) %>%
  mutate(
    `True Positive` = ifelse(is.na(`True Positive`), 0, `True Positive`),
    `True Negative` = ifelse(is.na(`True Negative`), 0, `True Negative`),
    `False Positive` = ifelse(is.na(`False Positive`), 0, `False Positive`),
    `False Negative` = ifelse(is.na(`False Negative`), 0, `False Negative`),
    BA = ((`True Positive` / (`True Positive` + `False Negative`)) + 
            (`True Negative` / (`True Negative` + `False Positive`))) / 2,
  ) 

# Make plots--------------------------------------------------------------------

# Check assumptions of ANOVA
Check <- Stats_Table %>% 
  select(Method, BA) %>%
  mutate(BA = log(BA)) %>%
  group_by(Method) %>%
  mutate(Residuals = BA - mean(BA))
plot(qqnorm(Check$Residuals))
qqline(Check$Residuals) # Normality assumption is ok
ggplot(Check, aes(x = Method, y = Residuals)) + geom_boxplot() + theme_bw() # Equal variance is ok

Stats_Table2 <- Stats_Table %>%
  mutate(BA = log(BA))
  
# Calculate an ANOVA and get the p-values for the multiple comparison adjustment 
myanova <- lm(BA~Method, data = Stats_Table2)
summary(myanova)
TukeyHSD(aov(BA~Method, data = Stats_Table2))$Method %>%
  data.frame() %>%
  arrange(p.adj)

mymets <- Stats_Table %>%
  group_by(Method) %>%
  summarise(Mean = mean(BA), SD = sd(BA)) %>%
  arrange(-Mean)
mymets

# Order plot 
Overview_Plot <- Stats_Table %>%
  mutate(Method = factor(Method, levels = c("clara", "supercells", "KCC", "k-means",
                                            "Multi-Otsu", "pytorch-tip", "binning", 
                                            "recolorize"))) %>%
    ggplot(aes(x = Method, y = BA)) + 
    geom_boxplot(outlier.shape = NA) +
    #geom_jitter(width = 0.1, height = 0, alpha = 0.25) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylim(c(0, 1.1)) +
    theme(legend.position = "none") +
    ylab("Balanced Accuracy") + 
    xlab("")
Overview_Plot

# Make a hierarchical cluster
HcluPlot <- Stats_Table %>%
  select(Cluster, Image, Method, BA) %>%
  mutate(BA = ifelse(is.na(BA), 0, BA)) %>%
  pivot_wider(id_cols = c(Cluster, Image), names_from = Method, values_from = BA) %>%
  select(-c(Cluster, Image)) %>%
  t() %>%
  dist() %>%
  hclust() %>%
  ggdendrogram(rotate = TRUE) +
  theme(axis.text.x = element_blank())
HcluPlot 

PerformancePlot <- rbind(
  left_join(
    Stats_Table %>% filter(Cluster == 1) %>% select(Image, Method, BA),
    Stats_Table %>% group_by(Image, Method) %>% summarize(`Number of Clusters` = n())
  ),
  left_join(
    Stats_Table %>% filter(Cluster != 1) %>% select(Image, Method, BA),
    Stats_Table %>% group_by(Image, Method) %>% summarize(`Number of Clusters` = n())
  ) 
) %>%
  mutate(
    Random = 1 / `Number of Clusters`,
    `Number of Clusters` = as.factor(paste("Number of Clusters:", `Number of Clusters`)),
  ) %>%
  rename(Model = Method) %>%
  ggplot(aes(x = Model, y = BA, fill = Model)) +
    geom_boxplot() +
    geom_hline(aes(yintercept = Random), color = "red") + 
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(.~`Number of Clusters`, nrow = 1) + 
    xlab("") +
    ylab("Balanced Accuracy") 

PerformancePlot

# Average time 
algOrder <- c("binning", "Multi-Otsu", "clara", "K-Means", "recolorize", "pytorch-tip", "supercells", "KCC")
SpeedPlot <- data.table(
  Algorithm = factor(algOrder, levels = algOrder),
  `Average Time (seconds)` = c(1.95, 2.5, 4.5, 5.8, 15.3, 20.2, 29.5, 92.3)
) %>%
  ggplot(aes(x = Algorithm, y = `Average Time (seconds)`)) +
    geom_bar(stat = "identity", color = "black", fill = "steelblue") +
    theme_bw() +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    xlab("")
SpeedPlot

# Figure 1
(DR_Plot | Overview_Plot | SpeedPlot | HcluPlot) / PerformancePlot + 
  plot_annotation(tag_levels = "A")

## Cluster Statistics

image_clus <- Stats_Table %>% 
  group_by(Image) %>% 
  slice_max(Cluster) %>%
  select(Image, Cluster) %>%
  unique()

Stats_Table %>%
  select(Image, BA) %>%
  group_by(Image) %>%
  summarize(Mean = mean(BA)) %>%
  left_join(image_clus) %>%
  group_by(Cluster) %>%
  summarize(Mean = mean(Mean))

Stats_Table %>%
  group_by(Method) %>%
  summarize(Mean = mean(BA), SD = sd(BA))







