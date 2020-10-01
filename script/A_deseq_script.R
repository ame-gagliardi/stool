library(tidyverse)
library(DESeq2)

count.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data/ngs/merged_harmonized_converted.rds") # Path delle conte
covar.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data/clinical/sensitivity/no_veg.rds")         # Path delle covariate

rawCount <- readRDS(count.path)
coldata <- as.data.frame(readRDS(covar.path))
rownames(coldata) <- coldata$id

# levels(coldata$bmi_cat) <- c("1","0","2","2") ## solo per analisi bmi
# coldata <- coldata[which(coldata$bmi_cat != "1"),]


keep <- which(coldata$alco_class != 0)
coldata <- coldata[keep,]

covars <- c("library", "sex","age_cat", "alco_class") # Modello per l'analisi, l'ultima variabile è quella d'interesse

i <- intersect(colnames(rawCount), rownames(coldata)) # Controlla che i campioni siano gli stessi e ordinati nello stesso modo
rawCount <- rawCount[,i]                              # nella matrice delle conte e nella tabella delle covariate                          
coldata <- coldata[i,]
all.equal(colnames(rawCount), rownames(coldata))

m <- length(coldata[,last(covars)])
coldata <- tidyr::drop_na(coldata, last(covars))
n <- length(coldata[,last(covars)])
n.na <- m-n                                          # Ti dice quanti campioni ha eliminato

if(n.na>0){
  i <- intersect(colnames(rawCount), rownames(coldata)) # Risistema i dataframe in modo che siano uguali
  rawCount <- rawCount[,i]                              
  coldata <- coldata[i,]
  all.equal(colnames(rawCount), rownames(coldata))
}


rm(covar.path,count.path,i,m,n)

model <- as.formula(paste("~", paste(covars,collapse = "+")))

# reduced <- as.formula(paste("~", paste(c("library","age_cat"),collapse = "+"))) # Modello ridotto se uso il test LRT

# Creo l'oggetto di DESeq

dds <- DESeqDataSetFromMatrix(countData = rawCount,   
                              colData = coldata,
                              design = model)

# Analisi LRT - I risultati alla fine si filtrano solo in base al p.adj
# dds_LRT <- DESeq(dds, test = "LRT", reduced = reduced)
# res_LRT <- results(dds_LRT)
# 
# sig_res_LRT <- res_LRT %>%
#   data.frame() %>%
#   rownames_to_column(var="miRNA") %>% 
#   as_tibble() %>% 
#   filter(padj < padj.cutoff) %>% 
#   arrange(padj)

# Analisi WALD - PREDEFINITA -  I risultati alla fine si possono filtrare sia per p.adj che per LFC

dds <- DESeq(dds)
resultsNames(dds)

res_1 <- results(dds, contrast = c(last(covars),"2","1"), alpha = 0.01)
res_1$miRNA <- rownames(res_1)

res_1 <- as_tibble(res_1) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "2_1")

res_2 <- results(dds, contrast = c(last(covars),"1", "0"), alpha = 0.01)
res_2$miRNA <- rownames(res_2)

res_2 <- as_tibble(res_2) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "1_0")

res_3 <- results(dds, contrast = c(last(covars),"2", "1"), alpha = 0.01)
res_3$miRNA <- rownames(res_3)

res_3 <- as_tibble(res_3) %>% 
  dplyr::relocate("miRNA", .before = "baseMean") %>% 
  dplyr::arrange(padj) %>% 
  dplyr::mutate(comparison = "2_1")

res <- bind_rows(res_1, res_2, res_3) %>% 
  dplyr::arrange(padj) %>% 
  dplyr::distinct(miRNA, .keep_all = TRUE)


save(res_1,  coldata, 
     file = paste0("C:/Users/amedeo/Desktop/R_Projects/stool/results/sensitivity_analysis/_celiac/",last(covars),".rds"))

rm(list=ls())
