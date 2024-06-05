library(tidyverse)
library(data.table)

#####################
## BLUR COMPARISON ##
#####################

# Load and format data
KM_Blur <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KMeans_Blur_Counts.csv")
KM <- fread("~/Git_Repos/UnsupervisedSegmentation/Performance/Counts/KMeans_Counts.csv") %>% 
  filter(Image %in% KM_Blur$Image)

# Calculate F1s
rbind(
  KM %>% mutate(Algorithm = "K-Means", Format = "Original"),
  KM_Blur %>% mutate(Algorithm = "K-Means", Format = "Blur")
) %>%
  pivot_wider(id_cols = c(Cluster, Image, Algorithm, Format), names_from = Counts, values_from = Freq) %>%
  mutate(
    Precision = `True Positive` / (`True Positive` + `False Positive`),
    Recall = `True Positive` / (`True Positive` + `False Negative`), 
    F1 = (2 * Precision * Recall) / (Precision + Recall)
  ) %>%
  select(Algorithm, Format, F1) %>%
  mutate(Format = factor(Format, levels = c("Original", "Blur"))) %>%
  ggplot(aes(x = Algorithm, y = F1, fill = Format)) +
    geom_boxplot() + 
    geom_jitter(width = 0.25, height = 0) +
    theme_bw() +
    ylim(c(0,1)) + 
    ylab("")



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




