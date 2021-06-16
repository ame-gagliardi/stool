library(tidyverse)
library(xlsx)

covariate <- c("coffee")
mirna <- read.delim("data/downstream/enrichment_mirna_list.csv", sep = ";")
enrichment_files <- list.files("D:/R_Projects/stool/results/enrichment_pooled/enrichment_table/") 
comparison <- unique(mirna$test[which(mirna$covariate == covariate)])

tmp_3 <- as.data.frame(matrix(NA, ncol = 12))
colnames(tmp_3) <- c("GS", "loss", "gene.tested", "coef", "std.err", "t.value", "p.value", "adj.p.val", "Target_Gene", 
                     "Library", "expression", "comparison")

for(i in 1:length(comparison)){
  
  file_to_read <- grep(covariate, enrichment_files)
  file_to_read <- enrichment_files[file_to_read]
  
  file_to_df <- grep(comparison[i], file_to_read)
  file_to_df <- file_to_read[file_to_df]
  
  for(k in 1:length(file_to_df)){
    
    tmp <- read.delim(paste0("D:/R_Projects/stool/results/enrichment_pooled/enrichment_table/", file_to_df[k]), header = T, sep = "\t")
    tmp[,"expression"] <- ifelse(str_detect(file_to_df[k], "down"), "down", "up")
    tmp[,"comparison"] <- comparison[i]
    
    tmp_2 <- tmp %>% 
      dplyr::select(GS, loss, gene.tested, coef, std.err, t.value, p.value, adj.p.val, Target_Gene, Library, expression, comparison)
    
    tmp_3 <- rbind(tmp_2, tmp_3)
  }
  
}

tmp_3 <- tmp_3 %>% 
  dplyr::filter(adj.p.val < 0.05) %>% 
  dplyr::filter(abs(coef) > 0.5) %>% 
  dplyr::filter(gene.tested >= 2)

enrichment_libraries <- c("c2.cp.kegg.v7.1", "c2.cp.reactome.v7.1", "c5.bp.v7.1")
tmp_3[,"library"] <- ifelse(tmp_3$Library == "c2.cp.kegg.v7.1", "KEGG",
                            ifelse(tmp_3$Library == "c2.cp.reactome.v7.1", "REACTOME", "GO"))
tmp_3[,"covariate"] <- covariate

tmp_3 <- tmp_3[,c("GS", "covariate", "expression", "comparison", "loss", "gene.tested", "coef", "std.err",
                  "t.value", "p.value", "adj.p.val", "Target_Gene", "library", "Library")]


enrichment_libraries <- c("KEGG", "REACTOME", "GO")

for(i in 1:length(enrichment_libraries)){
  
  nam <- paste(covariate, enrichment_libraries[i], sep = "_")
  assign(nam,
         tmp_3[which(tmp_3$library == enrichment_libraries[i]),])
  
}

 
wb <- createWorkbook()
sheet_GO <- createSheet(wb, "Enrichment_GO")
sheet_KEGG <- createSheet(wb, "Enrichment_KEGG")
sheet_REACTOME <- createSheet(wb, "Enrichment_REACTOME")

addDataFrame(coffee_GO, sheet=sheet_GO, startColumn=1, row.names=FALSE)
addDataFrame(coffee_KEGG, sheet=sheet_KEGG, startColumn=1, row.names=FALSE)
addDataFrame(coffee_REACTOME, sheet=sheet_REACTOME, startColumn=1, row.names=FALSE)


saveWorkbook(wb, paste0(covariate, "_enrichment.xlsx"))

rm(list=ls())

