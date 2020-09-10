library(org.Hs.eg.db)
library(DOSE)
library(pathview)
library(clusterProfiler)
library(AnnotationHub)
library(ensembldb)

res <- res[1:500,]
mirna <- res$gene
write.table(mirna, "C:/Users/amedeo/Desktop/demirna.txt", row.names = FALSE, quote = FALSE, sep = "\n")
target <- read.table("C:/Users/amedeo/Desktop/demirna_target.csv", sep = ",", header = TRUE)
xx <- which(target$validated != "")
target <- target[xx,]
target <- target[,c(1,2,3)]
colnames(target) <- c("gene", "refseqid", "symbol")

grch38 <- annotables::grch38

idx <- grch38$symbol %in% target$symbol
ids <- grch38[idx,]

non_duplicates <- which(duplicated(ids$symbol) == FALSE)
ids <- ids[non_duplicates, ] 

res_1 <- inner_join(res, target, by="gene")
non_duplicates <- which(duplicated(res_1$symbol) == FALSE)
res_1 <- res_1[non_duplicates,]


res_ids <- inner_join(res_1, ids, by=c("symbol"="symbol")) %>% 
  dplyr::select(gene, p.adjusted, symbol, ensgene, chr, description)


all_genes <- as.character(res_ids$ensgene)
sig <- dplyr::filter(res_ids, p.adjusted < 0.05)
sig_genes <- as.character(sig$ensgene)


ego <- enrichGO(gene = sig_genes, 
                universe = all_genes,
                keyType = "ENSEMBL",
                OrgDb = org.Hs.eg.db, 
                ont = "BP", 
                pAdjustMethod = "BH", 
                qvalueCutoff = 0.05, 
                readable = TRUE)

cluster_summary <- data.frame(ego)

write.csv(cluster_summary, "C:/Users/amedeo/Desktop/prova_go.csv")


dotplot(ego, showCategory = 50)
emapplot(ego, showCategory = 50)


install.packages("gprofiler2")
install.packages("treemap")
library(gprofiler2)
library(treemap)


gprofiler_res <- gost(query = sig_genes, 
                      organism = "hsapiens", 
                      ordered_query = F, 
                      exclude_iea = F, 
                      correction_method = "fdr",
                      custom_bg = all_genes)
