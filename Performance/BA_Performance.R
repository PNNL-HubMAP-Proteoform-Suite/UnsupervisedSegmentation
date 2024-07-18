library(tidyverse)
library(data.table)
library(patchwork)
library(ggsignif)
library(ggcorrplot)

#########################
## DIMENSION REDUCTION ##
#########################

KCC <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/KCC_Counts.csv") %>% mutate(Method = "KCC")
PCA <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/PCA_KCC_Counts.csv") %>% mutate(Method = "PCA & KCC")
tSNE <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/tSNE_KCC_Counts.csv") %>% mutate(Method = "tSNE & KCC")
SVD <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/DR_Counts/SVD_KCC_Counts.csv") %>% mutate(Method = "SVD & KCC")

# Calculate balanced accuracies  
DR_Table <- rbind(KCC, PCA, tSNE, SVD) %>% 
  pivot_wider(id_cols = c(Cluster, Image, Method), names_from = Counts, values_from = Freq) %>%
  mutate(
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

DR_Plot <- ggplot(DR_Table, aes(x = Method, y = BA)) +
  geom_boxplot() +
  theme_bw() +
  geom_signif(comparisons = list(c("KCC", "tSNE & KCC")), annotations = "***", textsize = 8) +
  theme_bw() +
  ylim(c(0,1.05)) + 
  ylab("Balanced Accuracy") +
  xlab("") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14))

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
PY <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyImSeg_Counts.csv")
PY_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Blur_Counts/PyImSeg_Blur_Counts.csv")

# Calculate balanced accuracy 
BA <- rbind(
  KM %>% mutate(Algorithm = "K-Means", Format = "Original"),
  KM_Blur %>% mutate(Algorithm = "KMeans", Format = "Blur"),
  KCC %>% mutate(Algorithm = "KCC", Format = "Original"),
  KCC_Blur %>% mutate(Algorithm = "KCC", Format = "Blur"),
  Clara %>% mutate(Algorithm = "Clara", Format = "Original"),
  Clara_Blur %>% mutate(Algorithm = "Clara", Format = "Blur"),
  Scell %>% mutate(Algorithm = "Supercells", Format = "Original"),
  Scell_Blur %>% mutate(Algorithm = "Supercells", Format = "Blur"),
  Re %>% mutate(Algorithm = "Recolorize", Format = "Original"),
  Re_Blur %>% mutate(Algorithm = "Recolorize", Format = "Blur"),
  PT %>% mutate(Algorithm = "pytorch-tip", Format = "Original"),
  PT_Blur %>% mutate(Algorithm = "pytorch-tip", Format = "Blur"),
  PY %>% mutate(Algorithm = "pyImSegm", Format = "Original"),
  PY_Blur %>% mutate(Algorithm = "pyImSegm", Format = "Blur")
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
    geom_boxplot() + 
    geom_signif(xmin = 2.8, xmax = 3.2, y_position = 1.01, annotation = "***") +
    theme_bw() +
    ylim(c(0,1.05)) + 
    ylab("Balanced Accuracy") +
    xlab("") +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 14))

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

DR_Plot + BA_Plot + plot_layout(widths = c(1,2)) + plot_annotation(tag_levels = "A")

###################
## FULL ANALYSIS ##
###################

# Load all files
all_counts <- do.call(rbind, lapply(list.files("~/Git_Repos/UnsupervisedSegmentation/Performance/Full_Counts/", full.names = T), function(file) {
  data <- fread(file)
  data$Method <- strsplit(file, "/", fixed = T) %>% unlist() %>% tail(1) %>% gsub(pattern = "_Counts.csv", replacement = "")
  return(data)
})) %>%
  mutate(Method = ifelse(Method == "KCC_Blur", "KCC with Blur", Method))

# Calculate balanced accuracies  
Stats_Table <- all_counts %>% 
  pivot_wider(id_cols = c(Cluster, Image, Method), names_from = Counts, values_from = Freq) %>%
  mutate(
    BA = ((`True Positive` / (`True Positive` + `False Negative`)) + 
            (`True Negative` / (`True Negative` + `False Positive`))) / 2,
  ) %>% mutate(
    Method = ifelse(Method == "PyImSeg", "pyImSegm", Method), 
    Method = ifelse(Method == "PyTorch", "pytorch-tip", Method)
  )
  
# Make plots--------------------------------------------------------------------

# Check assumptions of ANOVA
Check <- Stats_Table %>% 
  select(Method, BA) %>%
  filter(!is.na(BA)) %>%
  group_by(Method) %>%
  mutate(Residuals = BA - mean(BA))
plot(qqnorm(Check$Residuals))
qqline(Check$Residuals) # Normality assumption is ok
ggplot(Check, aes(x = Method, y = Residuals)) + geom_boxplot() + theme_bw() # Equal variance is ok

# Calculate an ANOVA and get the p-values for the multiple comparison adjustment 
myanova <- lm(BA~Method, data = Stats_Table)
summary(myanova)
TukeyHSD(aov(BA~Method, data = Stats_Table))

Stats_Table %>%
  group_by(Method) %>%
  summarise(`Median BA` = median(BA, na.rm = T)) %>%
  arrange(-`Median BA`)

# Order plot 
Overview_Plot <- Stats_Table %>%
  mutate(Method = factor(Method, levels = c("Recolorize", "KCC with Blur", "KMeans", 
                                            "Supercells", "pyImSegm", "Clara", "pytorch-tip"))) %>%
    ggplot(aes(x = Method, y = BA)) + 
    geom_boxplot() +
    geom_signif(comparisons = list(c("Clara", "KCC with Blur"), c("Clara", "Recolorize"),
                                   c("pytorch-tip", "KCC with Blur"), c("pytorch-tip", "Recolorize")),
                annotations = "*", textsize = 8) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)) +
    ylim(c(0, 1.1)) +
    theme(legend.position = "none") +
    ylab("Balanced Accuracy") + 
    xlab("")

Overview_Plot

# Make a correlation matrix
CorrPlot <- Stats_Table %>%
  select(Cluster, Image, Method, BA) %>%
  mutate(BA = ifelse(is.na(BA), 0, BA)) %>%
  pivot_wider(id_cols = c(Cluster, Image), names_from = Method, values_from = BA) %>%
  select(-c(Cluster, Image)) %>%
  cor(method = "pearson") %>%
  ggcorrplot(hc.order = TRUE, type = "full", lab = TRUE, legend.title = "Pearson\nCorrelation")
CorrPlot

PerformancePlot <- rbind(
  left_join(
    Stats_Table %>% filter(Cluster == 1) %>% select(Image, Method, BA),
    Stats_Table %>% group_by(Image, Method) %>% summarize(`Number of Clusters` = n())
  ) %>% mutate(Type = "Background Cluster"),
  left_join(
    Stats_Table %>% filter(Cluster != 1) %>% select(Image, Method, BA),
    Stats_Table %>% group_by(Image, Method) %>% summarize(`Number of Clusters` = n())
  ) %>% mutate(Type = "Feature Clusters")
) %>%
  mutate(`Number of Clusters` = as.factor(`Number of Clusters`)) %>%
  ggplot(aes(x = `Number of Clusters`, y = BA, fill = `Number of Clusters`)) +
    geom_boxplot() +
    theme_bw() +
    ylim(c(0, 1)) +
    theme(legend.position = "none") +
    facet_grid(cols = vars(Method), rows = vars(Type)) + 
    ylab("Balanced Accuracy") 

PerformancePlot

# Average time 
algOrder <- c("Clara", "KMeans", "Recolorize", "pyImSegm", "pytorch-tip", "Supercells", "KCC")
SpeedPlot <- data.table(
  Algorithm = factor(algOrder, levels = algOrder),
  `Average Time (seconds)` = c(4.5, 5.8, 15.3, 15.8, 20.2, 29.5, 92.3)
) %>%
  ggplot(aes(x = Algorithm, y = `Average Time (seconds)`)) +
    geom_bar(stat = "identity") +
    theme_bw() +
   theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)) +
    xlab("")
SpeedPlot

(Overview_Plot + SpeedPlot + CorrPlot) / PerformancePlot + plot_annotation(tag_levels = "A")






