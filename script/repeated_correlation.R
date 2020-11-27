source("C:/Users/amedeo/Desktop/R_Projects/general_script/useful_functions.R")
library(pheatmap)
library(RColorBrewer)
library(grid)
library(ggpubr)
library(ggrepel)

# Data

df <- readRDS("data/repeated_samples/repeated_df.rds")
cts <- readRDS("data/repeated_samples/repeated_cts_raw.rds")
norm<- readRDS("data/repeated_samples/repeated_cts_norm.rds")

# Heatmap of the correlation

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

rm(dds, vsd, vsd_mat, vsd_cor, breaksList)

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

vov_cts[,"vov.mean"] <- apply(vov_cts, 1, mean, na.rm = TRUE)
cel_cts[,"cel.mean"] <- apply(cel_cts, 1, mean, na.rm = TRUE)

xx <- cor.test(vov_cts$vov.mean, cel_cts$cel.mean)

whole_cts <- bind_cols(vov_cts, cel_cts)

# grob <- grobTree(textGrob(paste0("r = ", round(xx$estimate, 3)), x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
# grob2 <- grobTree(textGrob(paste0("p.value = ", xx$p.value), x=0.1,  y=0.9, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
# 
# p <- ggplot(whole_cts, aes(x = log(vov.mean + 1), y = log(cel.mean + 1))) +
#   geom_point() +
#   stat_cor(method = "pearson", label.x = 3, label.y = 30) +
#   labs(x = "Mean of Vov samples", y = "Mean of Celiac samples", title = "Correlation plot of miRNA in VOV and Celiac samples") +
#   annotation_custom(grob) +
#   annotation_custom(grob2)

whole_cts$x <- log(whole_cts$vov.mean + 1)
whole_cts$y <- log(whole_cts$cel.mean + 1)

p <- ggscatter(whole_cts, "x", "y",
               add = 'reg.line',
               add.params = list(color = "blue", fill = "lightgray"),
               conf.int = TRUE,
               xlab = "Mean of VOV samples",
               ylab = "Mean of Celiac samples",
               title = "Correlation plot of miRNA counts in VOV and Celiac samples",
               xlim = c(0,10),
               ylim = c(0,10))
p + stat_cor(method = "pearson", label.x = 3, label.y = 30)

  
  
  
  
  
## PCA plot

# Deseq

df[1:6, "sample"] <- c("Vov")
df[7:12, "sample"] <- c("Celiac")

dds <- DESeqDataSetFromMatrix(countData = cts,
                              colData = df,
                              design = ~age_cat + sex)

vsd <- varianceStabilizingTransformation(dds, blind=TRUE)

pcaData <- plotPCA(vsd, intgroup = "sample", returnData = TRUE)

p <- ggplot(pcaData, aes(x = PC1, y = PC2, color = sample)) +
  geom_point(size = 2) +
  geom_label_repel(aes(label = name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_bw()

ggsave("data/repeated_samples/figures/pca_deseq.jpg", p)

# Prcomp

count.pca <- prcomp(log(cts+1), center = TRUE, scale. = TRUE)
tmp <- as.data.frame(count.pca$rotation[,c(1,2,3,4)])
tmp[,"name"] <- rownames(tmp)
colnames(tmp) <- c("PC1","PC2","PC3","PC4", "name")

check <- all.equal(rownames(tmp), rownames(df))
if(check == FALSE){
  i <- intersect(rownames(tmp), rownames(df))
  tmp <- tmp[i,]
  coldata <- coldata[i,]
}

varPCA <- "sample"


tmp[,varPCA] <- df[match(rownames(tmp), rownames(df), nomatch=0),varPCA]
p <- ggplot2::ggplot(data = tmp, ggplot2::aes(x=PC1, y=PC2)) +
  geom_point(aes(color=get(varPCA))) + 
  scale_color_discrete(name = varPCA) + 
  geom_label_repel(aes(label = name),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 segment.color = 'grey50') +
  theme_bw()

ggsave("data/repeated_samples/figures/pca_prcomp.jpg", p)




