library(tidyverse)
library(ggrepel)

load("results/smoke/smoke.rda")

smoke <- res
smoke_sig <- res %>% 
  dplyr::filter(baseMean >=15 & padj < 0.05) # Prendo i significativi del fumo

rm(res, res_1, res_2, res_3)

load("results/ncigs/ncigs.rda")
ncigs <- res
ncigs_sig <- res %>% 
  dplyr::filter(baseMean >=15 & padj < 0.05) # Prendo i significativi del numero di sigarette

rm(res)


i <- setdiff(ncigs_sig$miRNA, smoke_sig$miRNA) # Prendo quelli significativi nel numero di sigarette ma non nel fumo

trend <- as.data.frame(smoke[which(smoke$miRNA %in% i),]) # Prendo i gli stessi miRNA che risultano non significativi nel fumo
rownames(trend) <- trend$miRNA
ncigs_sig <- as.data.frame(ncigs_sig)
rownames(ncigs_sig) <- ncigs_sig$miRNA

m <- intersect(rownames(trend), rownames(ncigs_sig))

trend <- as_tibble(trend[m,])
ncigs_sig <- as_tibble(ncigs_sig[m,])

comparison <- as_tibble(data.frame("miRNA" = trend$miRNA, "log2FC_smk" = trend$log2FoldChange, "log2FC_cig" = ncigs_sig$log2FoldChange,
                                   "padj_smk" = trend$padj, "padj_cig" = ncigs_sig$padj)) # DF con i due LFC
comparison$trend <- as_factor(comparison$log2FC_smk >0 & comparison$log2FC_cig >0 | comparison$log2FC_smk <0 & comparison$log2FC_cig <0)
levels(comparison$trend) <- c("opposite", "same")

p <- ggplot(comparison, aes(x=log2FC_smk, y=log2FC_cig)) + 
  geom_point(aes(color = trend)) +
  geom_text_repel(aes(label=miRNA), size = 3, data = comparison) +
  labs(title = "Log2FC of smoke and cigs/day", x = "Smoke LFC", y = "Cigs/day LFC")

p <- p +
  theme(plot.title=element_text(size=15, 
                                face="bold", 
                                color="black",
                                hjust=0.5,
                                lineheight=1.2))
        
  

ggsave("C:/Users/amedeo/Desktop/R_Projects/stool/figures/scatter_smk_cig.jpg", p, width = 8, height = 8)


write.table(comparison, "C:/Users/amedeo/Desktop/R_Projects/stool/results/mirna_smoke_ncigs_trend.txt", 
            quote = FALSE, sep = "\t", row.names = FALSE)
