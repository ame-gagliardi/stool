library(tidyverse)
library(DESeq)

var <- c("age_cat")
## Dataset load
df <- readRDS("data/clinical/de_merged_cleaned.rds")
summary <- table(df$age_cat)
summary
levName <- levels(df$age_cat)
## DEseq analysis results
dds <- readRDS(paste0("results/full_model/dds/",var,".rds"))
resultsNames(dds)
results(dds)
## Mean of each miRNA for each class
mean <- readRDS("data/miRNA_mean.rds")

res_1 <- results(dds, contrast = c(var, levName[2], levName[1]))
res_1$miRNA <- rownames(res_1)
res_1 <- as_tibble(res_1) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = paste0(levName[2],"_vs_",levName[1])) %>% 
  dplyr::mutate(size = paste0(summary[[2]],"_",summary[[1]])) %>% 
  dplyr::mutate(sig = baseMean >= 15 & padj < 0.05) 

res_1 <- as.data.frame(res_1)
rownames(res_1) <- res_1$miRNA  

i <- intersect(rownames(res_1), rownames(mean))
mean <- mean[i,]
all.equal(rownames(res_1), rownames(mean))

res_1 <- as_tibble(res_1) %>% 
  dplyr::mutate(mean_4060 = mean$`40_60`, mean_40 = mean$`_40`)


check <- as.data.frame(matrix(NA, nrow = 3524, ncol = 3, dimnames = list(c(res_1$miRNA),
                                                           c("log2FC", "difference", "check"))))

check$log2FC <- res_1$log2FoldChange > 0
check$difference <- (res_1$mean_4060 - res_1$mean_40) > 0
check$check <- check$log2FC == check$difference
