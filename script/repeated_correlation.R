source("C:/Users/amedeo/Desktop/R_Projects/general_script/useful_functions.R")
library(pheatmap)
library(RColorBrewer)
library(grid)

# Data

df <- readRDS("data/repeated_samples/repeated_df.rds")
cts <- readRDS("data/repeated_samples/repeated_cts_raw.rds")
norm<- readRDS("data/repeated_samples/repeated_cts_norm.rds")

# Heatmap corelation

dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = df,
                              design = ~age_cat + sex)

vsd <- varianceStabilizingTransformation(dds, blind=TRUE)

vsd_mat <- assay(vsd) # Extract the vsd matrix from PCA

vsd_cor <- cor(vsd_mat) # Compute pairwise correlation values

breaksList = seq(0, 1, by = 0.1)

pheatmap(vsd_cor,
         color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), 
         breaks = breaksList,
         fontsize_row = 5,
         fontsize_col = 5,
         show_colnames = TRUE,
         show_rownames = TRUE,
         cluster_rows = F,
         cluster_cols = F)

# Correlation R graph

corr.df <- as.data.frame(matrix(nrow = 6, ncol = 5))
colnames(corr.df) <- c("Person","Vov", "Cel", "pearson", "p.value")
corr.df$Person <- df$name[1:6]
corr.df$Vov <- rownames(df)[1:6]
corr.df$Cel <- rownames(df)[7:12]
rownames(corr.df) <- corr.df$Person

for(i in 1:6){
  yy <- cor.test(norm[,corr.df[i,2]], norm[,corr.df[i,3]])$estimate
  xx <- cor.test(norm[,corr.df[i,2]], norm[,corr.df[i,3]])$p.value
  corr.df[i,4] <- yy
  corr.df[i,5] <- xx
}
corr.df[,"name"] <- c("Luca Alessandri", "Alessia Russo", "Simonetta Guarrera",
                      "Barbara Pardini", "Alessio Naccarati", "Sonia Tarallo")
for(i in 1:6){
  
  cts_plot <- norm %>% 
  rownames_to_column() %>% 
  dplyr::rename(mirna = rowname) %>% 
  dplyr::select(mirna, corr.df[i, "Vov"], corr.df[i, "Cel"])
  
  grob <- grobTree(textGrob(paste0("r = ", round(corr.df[i,4], 3)), x=0.5,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob(paste0("p.value = ", round(corr.df[i,5], 10)), x=0.5,  y=0.90, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))
  
  p <- ggplot(cts_plot, aes(x= log(cts_plot[,2] + 1), y = log(cts_plot[,3] + 1))) +
  geom_point() + 
  labs(x = "Vov sample", y = "Celiac sample", title = corr.df[i, "name"]) +
  annotation_custom(grob) +
  annotation_custom(grob2)
  
  ggsave(paste0("C:/Users/amedeo/Desktop/R_Projects/stool/data/repeated_samples/figures/",
                corr.df[i, "name"],".jpg"), p)
}

vov_cts <- cts[,1:6]
cel_cts <- cts[,7:12]

vov_cts[,"mean"] <- apply(vov_cts, 1, mean, na.rm = TRUE)
cel_cts[,"mean"] <- apply(cel_cts, 1, mean, na.rm = TRUE)

cor.test(vov_cts$mean, cel_cts$mean)

# T-t test
