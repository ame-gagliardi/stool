library(tidyverse)
library(DESeq2)

# In questo script preparo le tabelle da salvare in txt per poi copiarle su excel.
# A seconda di quanti confronti ci sono salvo da 1 a 3 tabelle, cambiando di volta in volta il nome della variabile in esame e aggiungendo i dettagli che mi servono

var <- c("age_cat")
##
df <- readRDS("data/clinical/de_merged_cleaned.rds")
summary <- table(df$age_cat)
summary
##
dds <- readRDS(paste0("results/full_model/dds/",var,".rds"))
resultsNames(dds)
results(dds)
##
variability <- readRDS("data/variability.rds")
##
res_1 <- results(dds, contrast = c(var,"40_60", "_40"))
res_1$miRNA <- rownames(res_1)
res_1 <- as_tibble(res_1) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "4060_40") %>% 
  dplyr::mutate(size = paste0(summary[[2]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05) %>% 
  dplyr::mutate(median40 = variability$age_40, median4060 = variability$age40_60, median60 = variability$age60_) %>% 
  dplyr::mutate(mean40 = variability$age_40_mean, mean4060 = variability$age40_60_mean, mean60 = variability$age60__mean)
res_1
saveRDS(res_1, file = paste0("results/full_model/df_nofilter/",var,"_",res_1[1,8],"_full.rds"))
write.table(res_1, paste0("results/full_model/tables/",var,"_",res_1[1,8],"_full.txt"), quote = FALSE, row.names = FALSE, sep = "\t")

##

res_2 <- results(dds, contrast = c(var,"male", "female"))
res_2$miRNA <- rownames(res_2)
res_2 <- as_tibble(res_2) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "M_F") %>% 
  dplyr::mutate(size = paste0(summary[[2]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05) %>% 
  dplyr::mutate(male = variability$sexM, female = variability$sexF) %>% 
  dplyr::mutate(male_mean = variability$sexM_mean, female_mean = variability$sexF_mean)
res_2
saveRDS(res_2, file = paste0("results/full_model/df_nofilter/",var,"_",res_2[1,8],"_full.rds"))
write.table(res_2, paste0("results/full_model/tables/",var,"_",res_2[1,8],"_full.txt"), quote = FALSE, row.names = FALSE, sep = "\t")

##

res_3 <- results(dds, contrast = c(var,"male", "female"))
res_3$miRNA <- rownames(res_3)
res_3 <- as_tibble(res_3) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "M_F") %>% 
  dplyr::mutate(size = paste0(summary[[2]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05) %>% 
  dplyr::mutate(male = variability$sexM, female = variability$sexF) %>% 
  dplyr::mutate(male_mean = variability$sexM_mean, female_mean = variability$sexF_mean)
res_3
saveRDS(res_3, file = paste0("results/full_model/df_nofilter/",var,"_",res_3[1,8],"_full.rds"))
write.table(res_3, paste0("results/full_model/tables/",var,"_",res_1[1,8],"_full.txt"), quote = FALSE, row.names = FALSE, sep = "\t")

##

tot <- bind_rows(res_1, res_2, res_3) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)
tot
saveRDS(tot, file = paste0("results/full_model/df_nofilter/",var,"_tot_full.rds"))
write.table(tot, paste0("results/full_model/",var,"_tot_full.txt"), quote = FALSE, row.names = FALSE, sep = "\t")

##

rm(list=ls())

