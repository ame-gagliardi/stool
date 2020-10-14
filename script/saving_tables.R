library(tidyverse)

var <- c("phys_act")

df <- readRDS("C:/Users/amedeo/Desktop/R_Projects/stool/data/clinical/merged_cleaned.rds")
summary <- table(df$phys_act)
summary
##
dds <- readRDS(paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/dds/",var,".rds"))
resultsNames(dds)
##
res_1 <- results(dds, contrast = c(var,"1", "0"))
res_1$miRNA <- rownames(res_1)
res_1 <- as_tibble(res_1) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "ModIn_ModAct") %>% 
  dplyr::mutate(size = paste0(summary[[3]],"_",summary[[1]])) %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
res_1

write.table(res_1, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_1[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
res_2 <- results(dds, contrast = c(var,"2", "0"))
res_2$miRNA <- rownames(res_2)
res_2 <- as_tibble(res_2) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "In_ModAct")%>% 
  dplyr::mutate(size = paste0(summary[[3]],"_",summary[[1]]))
  dplyr::filter(baseMean >= 15 & padj < 0.05)
res_2
write.table(res_2, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_2[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
res_3 <- results(dds, contrast = c(var,"2", "1"))
res_3$miRNA <- rownames(res_3)
res_3 <- as_tibble(res_3) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "In_ModIn")%>% 
  dplyr::mutate(size = paste0(summary[[3]],"_",summary[[2]])) %>% 
  dplyr::filter(baseMean >= 15 & padj < 0.05)
res_3
write.table(res_3, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_",res_3[1,8],"_full.txt"), quote = FALSE, row.names = FALSE)
##
tot <- bind_rows(res_1, res_2, res_3) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)
tot
write.table(tot, paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/full_model/",var,"_unique_full.txt"), quote = FALSE, row.names = FALSE)
##
rm(list=ls())
