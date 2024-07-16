library(tidyverse)
library(data.table)
library(ggsignif)
library(ggcorrplot)

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
  mutate(Format = factor(Format, levels = c("Original", "Blur")),
         Cluster = as.factor(Cluster)) %>%
  ggplot(aes(x = Algorithm, y = BA, fill = Format)) +
    geom_boxplot() + 
    geom_signif(xmin = 2.8, xmax = 3.2, y_position = 1.01, annotation = "***") +
    theme_bw() +
    ylim(c(0,1.05)) + 
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
                                            "Supercells", "PyImSeg", "Clara", "PyTorch"))) %>%
    ggplot(aes(x = Method, y = BA, fill = Method)) + 
    geom_boxplot() +
    geom_signif(comparisons = list(c("Clara", "KCC with Blur"), c("Clara", "Recolorize"),
                                   c("PyTorch", "KCC with Blur"), c("PyTorch", "Recolorize")),
                annotations = "***", textsize = 8) +
    theme_bw() +
    ylim(c(0, 1.1)) +
    theme(legend.position = "none") +
    ylab("Balanced Accuracy") + 
    xlab("") + 
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 18))

Overview_Plot

# Make a correlation matrix
Stats_Table %>%
  select(Cluster, Image, Method, BA) %>%
  mutate(BA = ifelse(is.na(BA), 0, BA)) %>%
  pivot_wider(id_cols = c(Cluster, Image), names_from = Method, values_from = BA) %>%
  select(-c(Cluster, Image)) %>%
  cor(method = "pearson") %>%
  ggcorrplot(hc.order = TRUE, type = "full", lab = TRUE, legend.title = "Pearson\nCorrelation")





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






