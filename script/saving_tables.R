library(tidyverse)
library(DESeq2)

var <- c("sex")
##
df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/de_merged_cleaned.rds")
summary <- table(df$sex)
summary
##
dds <- readRDS(paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/dds/",var,".rds"))
resultsNames(dds)
results(dds)
##
variability <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/variability.rds")
##
res_1 <- results(dds, contrast = c(var,"male", "female"))
res_1$miRNA <- rownames(res_1)
res_1 <- as_tibble(res_1) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "M_F") %>% 
  dplyr::mutate(size = paste0(summary[[2]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05) %>% 
  dplyr::mutate(male = variability$sexM, female = variability$sexF) %>% 
  dplyr::mutate(male_mean = variability$sexM_mean, female_mean = variability$sexF_mean)
  res_1
saveRDS(res_1, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/",var,"_",res_1[1,8],"_full.rds"))
write.table(res_1, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_1[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
res_2 <- results(dds, contrast = c(var,"heavy_drinker", "no_coffee"))
res_2$miRNA <- rownames(res_2)
res_2 <- as_tibble(res_2) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "heavy_no")%>% 
  dplyr::mutate(size = paste0(summary[[3]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05)
res_2
saveRDS(res_2, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/",var,"_",res_2[1,8],"_full.rds"))
write.table(res_2, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_2[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
res_3 <- results(dds, contrast = c(var,"heavy_drinker", "light_drinker"))
res_3$miRNA <- rownames(res_3)
res_3 <- as_tibble(res_3) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "heavy_light")%>% 
  dplyr::mutate(size = paste0(summary[[3]],"_",summary[[2]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05)
res_3
saveRDS(res_3, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/",var,"_",res_3[1,8],"_full.rds"))
write.table(res_3, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_3[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
tot <- bind_rows(res_1, res_2, res_3) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)
tot
saveRDS(tot, file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/df_nofilter/",var,"_tot_full.rds"))
write.table(tot, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_tot_full.txt"), quote = FALSE, row.names = FALSE)
##
rm(list=ls())

