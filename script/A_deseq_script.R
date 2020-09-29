library(tidyverse)
library(DESeq2)

count.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data_redo/ngs/merged_harmonized_converted.rds") # Path delle conte
covar.path <- c("C:/Users/amedeo/Desktop/R_projects/stool/data_redo/clinical/merged_cleaned.rds")         # Path delle covariate

rawCount <- readRDS(count.path)
coldata <- readRDS(covar.path)

rm <- which(coldata$id == "VOV114")
coldata <- coldata[-rm,]

covars <- c("library","age_cat", "sex") # Modello per l'analisi, l'ultima variabile è quella d'interesse

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

reduced <- as.formula(paste("~", paste(c("library","age_cat"),collapse = "+"))) # Modello ridotto se uso il test LRT

# Creo l'oggetto di DESeq

dds <- DESeqDataSetFromMatrix(countData = rawCount,   
                              colData = coldata,
                              design = model)

# Analisi LRT - I risultati alla fine si filtrano solo in base al p.adj
dds_LRT <- DESeq(dds, test = "LRT", reduced = reduced)
res_LRT <- results(dds_LRT)

# Analisi WALD - PREDEFINITA -  I risultati alla fine si possono filtrare sia per p.adj che per LFC
dds_wald <- DESeq(dds)
res_wald <- results(dds_wald)
	

sig_res_LRT <- res_LRT %>%
  data.frame() %>%
  rownames_to_column(var="miRNA") %>% 
  as_tibble() %>% 
  filter(padj < padj.cutoff) %>% 
  arrange(padj)


sig_res_WALD <- res_wald %>%
  data.frame() %>%
  rownames_to_column(var="miRNA") %>% 
  as_tibble() %>% 
  filter(padj < padj.cutoff) %>% 
  arrange(padj)
