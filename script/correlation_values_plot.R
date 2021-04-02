source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_functions.R")
source("C:/Users/amedeo/Desktop/R_Projects/general_script/libraries_graph.R")

# DATA LOADING

r_folder <- getwd()
folder <- c("/data/")
project <- c("/sv_") 
tissue <- c("stool_")
biospecimen <- c("stool_")
sex <- c("female_")
ctsType <- c("normalized_")
species <- c("mirna_")
cohort <- c("pooled")
refVar <- c("BMI")
df.path <- paste0(r_folder, folder, "clinical", project, tissue, biospecimen, sex, "samples_", cohort, ".rds")
cts.path <- paste0(r_folder, folder, "ngs", project, tissue, biospecimen, sex ,ctsType, "counts_", species, cohort, ".rds")
result.path <- paste0(r_folder, "/results/differential")

df <- readRDS(df.path)
cts <- readRDS(cts.path)

## Correlation Dataset

both <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/correlation/pooled/stool_stool_both_bmi_pooled_correlation.rds")
male <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/correlation/pooled/stool_stool_male_bmi_pooled_correlation.rds")
female <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/results/correlation/pooled/stool_stool_female_bmi_pooled_correlation.rds")

both <- both %>% 
  dplyr::select(miRNA,SC, SC.p, SC.fdr) %>% 
  dplyr::mutate(index = seq(1:length(rownames(both)))) %>% 
  dplyr::mutate(fdr.treshold = ifelse(SC.fdr < 0.05, T,F)) %>% 
  dplyr::mutate(fdr.treshold = ifelse(SC.fdr < 0.05, T,F), dataset = "Both")

male <- male %>% 
  dplyr::select(miRNA,SC, SC.p, SC.fdr) %>% 
  dplyr::mutate(index = seq(1:length(rownames(male)))) %>% 
  dplyr::mutate(fdr.treshold = ifelse(SC.fdr < 0.05, T,F)) %>% 
  dplyr::mutate(fdr.treshold = ifelse(SC.fdr < 0.05, T,F), dataset = "Male")

female <- female %>% 
  dplyr::select(miRNA,SC, SC.p, SC.fdr) %>% 
  dplyr::mutate(index = seq(1:length(rownames(female)))) %>% 
  dplyr::mutate(fdr.treshold = ifelse(SC.fdr < 0.05, T,F), dataset = "Female")

a <- ggplot(both, aes(x = index, y = SC, color = fdr.treshold)) +
  geom_point() +
  geom_hline(yintercept = -0.2) +
  geom_hline(yintercept = 0.2) +
  ylim(-0.8, 0.8) +
  scale_color_discrete(name = "FDR", labels = c("<0.05", "Not significant")) +
  ggrepel::geom_label_repel(aes(label = ifelse(SC < -0.2 & SC.fdr < 0.05 | SC > 0.2 & SC.fdr < 0.05 , rownames(both), "")), color = "blue",
                            min.segment.length = 0, box.padding = 0.5) +
  labs(title = paste0(firstup(refVar), " Spearman correlation in both sexes"), y = "Spearman correlation", x = "miRNA index") +
  theme_classic()

m <- ggplot(male, aes(x = index, y = SC, color = fdr.treshold)) +
  geom_point() +
  geom_hline(yintercept = -0.2) +
  geom_hline(yintercept = 0.2) +
  ylim(-0.8, 0.8) +
  scale_color_discrete(name = "FDR", labels = c("<0.05", "Not significant")) +
  ggrepel::geom_label_repel(aes(label = ifelse(SC < -0.2 & SC.fdr < 0.05 | SC > 0.2 & SC.fdr < 0.05 , rownames(both), "")), color = "blue",
                            min.segment.length = 0, box.padding = 0.5) +
  labs(title = paste0(firstup(refVar), " Spearman correlation in males"), y = "Spearman correlation", x = "miRNA index") +
  theme_classic()

f <- ggplot(female, aes(x = index, y = SC, color = fdr.treshold)) +
  geom_point() +
  geom_hline(yintercept = -0.2) +
  geom_hline(yintercept = 0.2) +
  ylim(-0.8, 0.8) +
  scale_color_discrete(name = "FDR", labels = c("<0.05", "Not significant")) +
  ggrepel::geom_label_repel(aes(label = ifelse(SC < -0.2 & SC.fdr < 0.05 | SC > 0.2 & SC.fdr < 0.05 , rownames(both), "")), color = "blue",
                            min.segment.length = 0, box.padding = 0.5) +
  labs(title = paste0(firstup(refVar), " Spearman correlation in females"), y = "Spearman correlation", x = "miRNA index") +
  theme_classic()



ggsave(p, filename = paste0("results/figures/", refVar, "_both_", refRes, "_correlation.jpg"), width = 12, height = 14, units = "in")

rm(list=ls())


  
