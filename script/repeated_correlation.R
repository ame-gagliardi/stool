library(tidyverse)
library(DESeq2)

cts_vov <- readRDS("data/ngs/vov_cts_repeated.rds")
cts_celiac <- readRDS("data/ngs/celiac_cts_repeated.rds")
cts <- readRDS("data/ngs/repeated_counts.rds")
norm <- readRDS("data/ngs/repeated_normalized_counts.rds")

df <- readRDS("data/clinical/repeated_samples.rds")

i <- intersect(rownames(df), colnames(cts))
df <- df[i,]
cts <- cts[,i]

dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = df,
                              design = ~age_cat + sex)

vst <- varianceStabilizingTransformation(dds, blind=TRUE)

vst_mat <- assay(vst) # Extract the vst matrix from PCA

vst_cor <- cor(vst_mat) # Compute pairwise correlation values

breaksList = seq(0, 1, by = 0.1)

pheatmap(vst_cor,
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), 
         breaks = breaksList,
         fontsize_row = 5,
         fontsize_col = 5,
         show_colnames = TRUE,
         show_rownames = TRUE)
         cluster_rows = F,
         cluster_cols = F)
